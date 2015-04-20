module Paths_website (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rushil/cheap-game-finder/website/.cabal-sandbox/bin"
libdir     = "/home/rushil/cheap-game-finder/website/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/website-0.1.0.0"
datadir    = "/home/rushil/cheap-game-finder/website/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/website-0.1.0.0"
libexecdir = "/home/rushil/cheap-game-finder/website/.cabal-sandbox/libexec"
sysconfdir = "/home/rushil/cheap-game-finder/website/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "website_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "website_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "website_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "website_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "website_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
