{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_advent (
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

bindir     = "/home/tobias/.cabal/bin"
libdir     = "/home/tobias/.cabal/lib/x86_64-linux-ghc-8.10.1/haskell-advent-0.1.0.0-inplace-haskell-advent"
dynlibdir  = "/home/tobias/.cabal/lib/ghc-8.10.1/haskell-advent-0.1.0.0-inplace-haskell-advent"
datadir    = "/home/tobias/.cabal/share/x86_64-linux-ghc-8.10.1/haskell-advent-0.1.0.0"
libexecdir = "/home/tobias/.cabal/libexec/x86_64-linux-ghc-8.10.1/haskell-advent-0.1.0.0"
sysconfdir = "/home/tobias/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_advent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_advent_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_advent_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_advent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_advent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_advent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
