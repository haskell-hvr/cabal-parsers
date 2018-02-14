{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Tar        as Tar
import qualified Codec.Archive.Tar.Entry  as Tar
import qualified Codec.Compression.GZip   as GZip
import           Control.Exception
import           Control.Monad
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC8
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import           Data.Word
import           System.Directory
import           System.Environment
import           System.FilePath

import           Data.List                (isInfixOf, isPrefixOf)

import           Cabal.Parser.V200


-- | Read tarball lazily (and possibly decompress)
readTarEntries :: FilePath -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case takeExtension idxtar of
            ".gz"  -> Tar.read . GZip.decompress <$> BSL.readFile idxtar
            ".tar" -> Tar.read                   <$> BSL.readFile idxtar
            ext    -> error ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

filterEnts :: [Tar.Entry] -> [(FilePath,ByteString)]
filterEnts [] = []
filterEnts (e:es) =case Tar.entryContent e of
                     Tar.NormalFile bsl _sz | takeExtension efn == ".cabal" -> let bs = BSL.toStrict bsl in bs `seq` ((efn,bs) : filterEnts es)
                     _ -> filterEnts es
    where
      efn = Tar.entryPath e

decUtf8 :: ByteString -> String
decUtf8 raw = case T.unpack t of
                '\65279':cs -> cs
                cs          -> cs
  where
    t = T.decodeUtf8With T.lenientDecode raw

parseCab :: ByteString -> GenericPackageDescription
parseCab raw = case parseGenericPackageDescription (decUtf8 raw) of
                        ParseOk _ gpd -> gpd
                        _             -> error "parseCab"

main :: IO ()
main = do
    putStrLn "working..."
    cabdir <- getAppUserDataDirectory "cabal"
    ents <- readTarEntries (cabdir </> "packages" </> "hackage.haskell.org" </> "01-index.tar")

    forM_ ents $ \e -> do
        case Tar.entryContent e of
          Tar.NormalFile bsl _sz
              | takeExtension (Tar.entryPath e) == ".cabal" -> do
                    let raw = BSL.toStrict bsl
                    _ <- evaluate raw

                    -- print (Tar.entryPath e)

                    -- let sv'' = findCabVer raw
                    -- print =<< timeSeq (sv'')
                    -- print =<< timeSeq (strCaseStrAll raw "cabal-version")
                    -- putStrLn "--"

                    -- let sv3 = findCabVer2 raw

                    -- sv' <- evaluate $ parseSpecVer raw
                    when True $ do
                      case parseGenericPackageDescription (decUtf8 raw) of
                        ParseOk _ gpd -> do
                            -- let svr = specVersionRaw $ packageDescription gpd
                            --     sv  = specVersion $ packageDescription gpd
                            -- print (Tar.entryTime e, Tar.entryPath e)
                            pure ()

                        ParseFailed perr -> do
                            putStrLn "*** WOAH!"
                            print (Tar.entryTime e, Tar.entryPath e)
                            putStrLn ("parser fail: " ++ show perr)

              | otherwise -> pure ()

          _ -> pure () -- ignore all other entries

