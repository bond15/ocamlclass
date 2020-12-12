{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw1617 (
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

bindir     = "/Users/bond/.cabal/bin"
libdir     = "/Users/bond/.cabal/lib/x86_64-osx-ghc-8.10.1/hw1617-0.1.0.0-inplace-hw1617"
dynlibdir  = "/Users/bond/.cabal/lib/x86_64-osx-ghc-8.10.1"
datadir    = "/Users/bond/.cabal/share/x86_64-osx-ghc-8.10.1/hw1617-0.1.0.0"
libexecdir = "/Users/bond/.cabal/libexec/x86_64-osx-ghc-8.10.1/hw1617-0.1.0.0"
sysconfdir = "/Users/bond/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw1617_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw1617_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw1617_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw1617_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1617_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1617_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
