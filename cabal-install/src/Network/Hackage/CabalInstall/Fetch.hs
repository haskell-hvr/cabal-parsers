-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Fetch
    (
     -- * Commands
     fetch
    , -- * Utilities
      fetchPackage
    , packageFile
    , packagesDirectory
    , isFetched
    , readURI
    , downloadIndex
    ) where

import Network.URI (URI,parseURI,uriScheme,uriPath)
import Network.HTTP (ConnError(..), Request (..), simpleHTTP
                           , Response(..), RequestMethod (..))

import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.Version
import Text.Printf (printf)
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen (..), UnresolvedDependency (..))
import Network.Hackage.CabalInstall.Config (packagesDirectory)
import Network.Hackage.CabalInstall.Dependency (filterFetchables, resolveDependencies)

import Distribution.Package (PackageIdentifier(..), showPackageId)
import System.FilePath ((</>), (<.>))
import System.Directory (copyFile)
import System.IO (IOMode(..), hPutStr, Handle, hClose, openBinaryFile)
import Text.ParserCombinators.ReadP (readP_to_S)
import Distribution.ParseUtils (parseDependency)


readURI :: URI -> IO String
readURI uri
    | uriScheme uri == "file:" = (readFile $ uriPath uri)
    | otherwise = do
        eitherResult <- simpleHTTP (Request uri GET [] "")
        case eitherResult of
           Left err -> fail $ printf "Failed to download '%s': %s" (show uri) (show err)
           Right rsp
               | rspCode rsp == (2,0,0) -> return (rspBody rsp)
               | otherwise -> fail $ "Failed to download '" ++ show uri ++ "': Invalid HTTP code: " ++ show (rspCode rsp)

downloadURI :: FilePath -- ^ Where to put it
            -> URI      -- ^ What to download
            -> IO (Maybe ConnError)
downloadURI path uri
    | uriScheme uri == "file:" = do
        copyFile (uriPath uri) path
        return Nothing
    | otherwise = do
        eitherResult <- simpleHTTP request
        case eitherResult of
           Left err -> return (Just err)
           Right rsp
               | rspCode rsp == (2,0,0) -> withBinaryFile path WriteMode (`hPutStr` rspBody rsp) 
				                          >> return Nothing
               | otherwise -> return (Just (ErrorMisc ("Invalid HTTP code: " ++ show (rspCode rsp))))
    where request = Request uri GET [] ""



downloadFile :: FilePath
             -> String
             -> IO (Maybe ConnError)
downloadFile path url
    = case parseURI url of
        Just parsed -> downloadURI path parsed
        Nothing -> return (Just (ErrorMisc ("Failed to parse url: " ++ show url)))


-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: ConfigFlags -> PackageIdentifier -> String -> IO String
downloadPackage cfg pkg url
    = do message (configOutputGen cfg) 2 $ "GET " ++ show url
         mbError <- downloadFile path url
         case mbError of
           Just err -> fail $ printf "Failed to download '%s': %s" (showPackageId pkg) (show err)
           Nothing -> return path
    where path = packageFile cfg pkg

-- Downloads an index file to [config-dir/packages/serv-id
downloadIndex :: ConfigFlags -> String -> IO String
downloadIndex cfg serv
    = do createDirectoryIfMissing True (packagesDirectory cfg)
         mbError <- downloadFile path url
         case mbError of
           Just err -> fail $ printf "Failed to download index '%s'" (show err)
           Nothing  -> return path
    where url = serv ++ "/" ++ "00-index.tar.gz"
          path = packagesDirectory cfg </> "00-index" <.> "tar.gz"

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: ConfigFlags -> PackageIdentifier -> FilePath
packageFile cfg pkg = packagesDirectory cfg 
                      </> pkgName pkg
                      </> showVersion (pkgVersion pkg)
                      </> showPackageId pkg 
                      <.> "tar.gz"

-- |Returns @True@ if the package has already been fetched.
isFetched :: ConfigFlags -> PackageIdentifier -> IO Bool
isFetched cfg pkg
    = doesFileExist (packageFile cfg pkg)

-- |Fetch a package if we don't have it already.
fetchPackage :: ConfigFlags -> PackageIdentifier -> String -> IO String
fetchPackage cfg pkg location
    = do createDirectoryIfMissing True (packagesDirectory cfg)
         fetched <- isFetched cfg pkg
         if fetched
            then do pkgIsPresent (configOutputGen cfg) pkg
                    return (packageFile cfg pkg)
            else do downloadingPkg (configOutputGen cfg) pkg
                    downloadPackage cfg pkg location

-- |Fetch a list of packages and their dependencies.
fetch :: ConfigFlags -> [String] -> IO ()
fetch cfg pkgs
    = do apkgs <- fmap filterFetchables (resolveDependencies cfg [] (map parseDep pkgs))
         mapM_ (\(pkg,location)
                    -> fetchPackage cfg pkg location
               ) =<< filterM isNotFetched apkgs
    where parseDep dep
              = case readP_to_S parseDependency dep of
                 [] -> error ("Failed to parse package dependency: " ++ show dep)
                 x  -> UnresolvedDependency
                       { dependency = (fst (last x))
                       , depOptions = [] }
          isNotFetched (pkg,_location)
              = do fetched <- isFetched cfg pkg
                   pkgIsPresent output pkg
                   return (not fetched)
          output = configOutputGen cfg

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile name mode = bracket (openBinaryFile name mode) hClose