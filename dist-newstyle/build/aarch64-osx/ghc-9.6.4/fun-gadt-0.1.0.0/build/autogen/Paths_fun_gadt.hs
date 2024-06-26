{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fun_gadt
  
  (
    version,
    getBinDir,
    getLibDir,
    getDynLibDir,
    getLibexecDir,
    getDataFileName,
    getDataDir,
    getSysconfDir
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




bindir     :: FilePath
bindir     = "/Users/gobmeboul/.cabal/bin"
getBinDir     :: IO FilePath
getBinDir     = catchIO (getEnv "fun_gadt_bindir")     (\_ -> return bindir)

libdir     :: FilePath
libdir     = "/Users/gobmeboul/.cabal/lib/aarch64-osx-ghc-9.6.4/fun-gadt-0.1.0.0-inplace"
getLibDir     :: IO FilePath
getLibDir     = catchIO (getEnv "fun_gadt_libdir")     (\_ -> return libdir)

dynlibdir  :: FilePath
dynlibdir  = "/Users/gobmeboul/.cabal/lib/aarch64-osx-ghc-9.6.4"
getDynLibDir  :: IO FilePath
getDynLibDir  = catchIO (getEnv "fun_gadt_dynlibdir")  (\_ -> return dynlibdir)

datadir    :: FilePath
datadir    = "/Users/gobmeboul/.cabal/share/aarch64-osx-ghc-9.6.4/fun-gadt-0.1.0.0"
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "fun_gadt_datadir")    (\_ -> return datadir)

libexecdir :: FilePath
libexecdir = "/Users/gobmeboul/.cabal/libexec/aarch64-osx-ghc-9.6.4/fun-gadt-0.1.0.0"
getLibexecDir :: IO FilePath
getLibexecDir = catchIO (getEnv "fun_gadt_libexecdir") (\_ -> return libexecdir)

sysconfdir :: FilePath
sysconfdir = "/Users/gobmeboul/.cabal/etc"
getSysconfDir :: IO FilePath
getSysconfDir = catchIO (getEnv "fun_gadt_sysconfdir") (\_ -> return sysconfdir)



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
