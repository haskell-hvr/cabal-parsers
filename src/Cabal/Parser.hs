{-# LANGUAGE ViewPatterns #-}

module Cabal.Parser
    ( compatParseGenericPackageDescription
    , GenericPackageDescription
    , Version, PError, PWarning
    ) where

import           Control.DeepSeq
import           Control.Monad

import           Data.ByteString                        (ByteString)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Data.Text.Encoding.Error               as T

import           Distribution.PackageDescription        (GenericPackageDescription (..),
                                                         specVersion)
import           Distribution.PackageDescription.Parsec (parseGenericPackageDescription,
                                                         runParseResult)
import           Distribution.Parsec.Error              (PError (..))
import           Distribution.Parsec.Position           (Position (Position), zeroPos)
import           Distribution.Parsec.Warning            (PWarning (..))
import           Distribution.Version                   (Version, mkVersion)

import qualified Cabal.Parser.V118                      as V118
import qualified Cabal.Parser.V120                      as V120
import qualified Cabal.Parser.V122                      as V122
import qualified Cabal.Parser.V124                      as V124
import qualified Cabal.Parser.V200                      as V200
import qualified Cabal.Parser.V220                      as V220

unpackUTF8 :: ByteString -> String
unpackUTF8 raw = case T.unpack t of
                '\65279':cs -> cs
                cs          -> cs
  where
    t = T.decodeUtf8With T.lenientDecode raw

-- | Augmented version of 'runParseResult . parseGenericPackageDescription' which additional compatibility checks
--
--
compatParseGenericPackageDescription :: ByteString -> ([PWarning], Either (Maybe Version, [PError]) GenericPackageDescription)
compatParseGenericPackageDescription bs = case runParseResult (parseGenericPackageDescription bs) of
                                            pnok@(_, Left _) -> pnok -- error
                                            pok@(_pwarns, Right gpd) ->
                                              let sv = specVersion (packageDescription gpd)
                                              in case go sv of
                                                Nothing    -> pok
                                                Just perrs -> force ([], Left (Just sv, perrs))
 where
   go v = msum $ [ goV220 | v < mkVersion [2,3]  ]
              ++ [ goV200 | v < mkVersion [2,1]  ]
              ++ [ goV124 | v < mkVersion [1,25] ]
              -- NB: cabal spec versions prior to cabal-version:2.0 need to be parseable by older parsers as well
              ++ [ goV122 | v < mkVersion [1,25] ]
              ++ [ goV120 | v < mkVersion [1,25] ]
              ++ [ goV118 | v < mkVersion [1,25] ]

   mlno2pos :: Maybe Int -> Position
   mlno2pos Nothing    = zeroPos
   mlno2pos (Just lno) = Position lno 0

   goV220 :: Maybe [PError]
   goV220 = case V220.runParseResult $ V220.parseGenericPackageDescription bs of
              (_pwarns, Right _)   -> Nothing
              (_, Left (_, perrs)) -> Just (map convertPerr perrs)
     where
       convertPerr :: V220.PError -> PError
       convertPerr (V220.PError (V220.Position lno cno) msg) = PError pos ("[v2.2] " ++ msg)
         where
           pos = Position lno cno

   goV200 :: Maybe [PError]
   goV200 = case V200.parseGenericPackageDescription (unpackUTF8 bs) of
              V200.ParseFailed perr -> Just [convertPerr perr]
              V200.ParseOk _ _      -> Nothing
     where
       convertPerr :: V200.PError -> PError
       convertPerr pe = PError pos ("[v2.0] " ++ msg)
         where
           (mlno2pos -> pos, msg) = V200.locatedErrorMsg pe

   goV124 :: Maybe [PError]
   goV124 = case V124.parsePackageDescription (unpackUTF8 bs) of
              V124.ParseFailed perr -> Just [convertPerr perr]
              V124.ParseOk _ _      -> Nothing
     where
       convertPerr :: V124.PError -> PError
       convertPerr pe = PError pos ("[v1.24] " ++ msg)
         where
           (mlno2pos -> pos, msg) = V124.locatedErrorMsg pe

   goV122 :: Maybe [PError]
   goV122 = case V122.parsePackageDescription (unpackUTF8 bs) of
              V122.ParseFailed perr -> Just [convertPerr perr]
              V122.ParseOk _ _      -> Nothing
     where
       convertPerr :: V122.PError -> PError
       convertPerr pe = PError pos ("[v1.22] " ++ msg)
         where
           (mlno2pos -> pos, msg) = V122.locatedErrorMsg pe

   goV120 :: Maybe [PError]
   goV120 = case V120.parsePackageDescription (unpackUTF8 bs) of
              V120.ParseFailed perr -> Just [convertPerr perr]
              V120.ParseOk _ _      -> Nothing
     where
       convertPerr :: V120.PError -> PError
       convertPerr pe = PError pos ("[v1.20] " ++ msg)
         where
           (mlno2pos -> pos, msg) = V120.locatedErrorMsg pe

   goV118 :: Maybe [PError]
   goV118 = case V118.parsePackageDescription (unpackUTF8 bs) of
              V118.ParseFailed perr -> Just [convertPerr perr]
              V118.ParseOk _ _      -> Nothing
     where
       convertPerr :: V118.PError -> PError
       convertPerr pe = PError pos ("[v1.20] " ++ msg)
         where
           (mlno2pos -> pos, msg) = V118.locatedErrorMsg pe
