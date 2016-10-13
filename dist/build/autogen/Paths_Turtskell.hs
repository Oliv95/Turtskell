module Paths_Turtskell (
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

bindir     = "/home/oliv/.cabal/bin"
libdir     = "/home/oliv/.cabal/lib/x86_64-linux-ghc-7.10.3/Turtskell-0.1.0.0-FiYi2vrc50FBkY6obIaiPQ"
datadir    = "/home/oliv/.cabal/share/x86_64-linux-ghc-7.10.3/Turtskell-0.1.0.0"
libexecdir = "/home/oliv/.cabal/libexec"
sysconfdir = "/home/oliv/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Turtskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Turtskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Turtskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Turtskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Turtskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
