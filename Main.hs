module Main where

import           Cabal.Parser.V200
import           Control.Monad
import           System.Environment

main :: IO ()
main = do
  argv <- getArgs

  forM_ argv $ \fn -> do
    s <- readFile fn
    case parseGenericPackageDescription s of
      ParseOk warnings gpd -> do
        putStrLn $ concat ["OK (", show (length warnings), " warnings)"]
      ParseFailed perr -> do
        putStrLn $ "FAILED"
        print perr

    pure ()

  return ()
