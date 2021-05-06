{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_morra (
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

bindir     = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/bin"
libdir     = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/lib/x86_64-linux-ghc-8.10.4/morra-0.1.0.0-EJJAoGL7ZUn8A87Ed90UhW-morra"
dynlibdir  = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/share/x86_64-linux-ghc-8.10.4/morra-0.1.0.0"
libexecdir = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/libexec/x86_64-linux-ghc-8.10.4/morra-0.1.0.0"
sysconfdir = "/home/dokkora/fun/learning-haskell/morra/.stack-work/install/x86_64-linux-nix/8557864d76752e88b53a369897f3f60017d577cb583c9cd92bf8cd21871c6f67/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morra_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morra_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "morra_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "morra_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morra_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
