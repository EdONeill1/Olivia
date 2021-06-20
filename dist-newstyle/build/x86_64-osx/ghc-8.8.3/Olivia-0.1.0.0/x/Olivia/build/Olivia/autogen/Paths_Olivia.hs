{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Olivia (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/edward/.cabal/bin"
libdir     = "/Users/edward/.cabal/lib/x86_64-osx-ghc-8.8.3/Olivia-0.1.0.0-inplace-Olivia"
dynlibdir  = "/Users/edward/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/edward/.cabal/share/x86_64-osx-ghc-8.8.3/Olivia-0.1.0.0"
libexecdir = "/Users/edward/.cabal/libexec/x86_64-osx-ghc-8.8.3/Olivia-0.1.0.0"
sysconfdir = "/Users/edward/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Olivia_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Olivia_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Olivia_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Olivia_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Olivia_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Olivia_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
