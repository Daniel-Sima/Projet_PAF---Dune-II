{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_paf_projet (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/bin"
libdir     = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/lib/x86_64-linux-ghc-9.2.5/paf-projet-0.1.0.0-3wD18M0J0JjGk46EEOaV3n-paf-projet-test"
dynlibdir  = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/share/x86_64-linux-ghc-9.2.5/paf-projet-0.1.0.0"
libexecdir = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/libexec/x86_64-linux-ghc-9.2.5/paf-projet-0.1.0.0"
sysconfdir = "/home/yukhoi/Projet_PAF---Dune-II/.stack-work/install/x86_64-linux-tinfo6/e0aaf7f4d8c4f29b5a1f088e79e6e76416c3a9f8798d647a372cb55c3f00a099/9.2.5/etc"

getBinDir     = catchIO (getEnv "paf_projet_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "paf_projet_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "paf_projet_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "paf_projet_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "paf_projet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "paf_projet_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
