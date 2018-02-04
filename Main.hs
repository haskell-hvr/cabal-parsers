{-# OPTIONS_GHC -Wall #-}

module Main where

import           Cabal.Parser
import           Control.Monad
import qualified Data.ByteString    as B
import           System.Environment

main :: IO ()
main = do
  argv <- getArgs

  forM_ argv $ \fn -> do
    s <- B.readFile fn
    case compatParseGenericPackageDescription s of
      ParseOk warnings _gpd -> do
        putStrLn $ concat ["OK (", show (length warnings), " warnings)"]
      ParseFailed perr -> do
        putStrLn $ "FAILED"
        print perr

  return ()
