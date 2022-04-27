{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Raytracer (
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

bindir     = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.2\\Raytracer-0.1.0.0-inplace-Raytracer"
dynlibdir  = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.2\\Raytracer-0.1.0.0"
libexecdir = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\Raytracer-0.1.0.0-inplace-Raytracer\\x86_64-windows-ghc-9.0.2\\Raytracer-0.1.0.0"
sysconfdir = "C:\\Users\\manam\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Raytracer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Raytracer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Raytracer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Raytracer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Raytracer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Raytracer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
