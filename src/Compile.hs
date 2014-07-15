{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
--
-- Module to compile effects using libGHC
--
module Compile where

-- standard libraries
import System.Plugins as Plugins
import System.Plugins.Utils (mkTemp)
import System.Directory
import System.FilePath
import System.IO
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)
import qualified Shady.Color as S
import qualified Shady.CompileEffect as S

-- FIXME: Don't hard code package database
packageDB = "/Users/sseefried/code/mac-shady-project/MacShady/.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d"

-- | Compile the effect code. Return either the @Effect@ or a compiler error (Left).
--
compileAndLoadEffect :: FilePath -> Int -> IO (Either String S.GLSLEffect)
compileAndLoadEffect path i = do
  (res, name) <- makeEffect path
  case res of
     Right objectFile -> loadAndUpdateEffect objectFile name
     Left errors      -> return (Left $ formatErrors errors)
  where
    formatErrors errors = concat $ intersperse "\n" errors
    --
    -- Load the plugin, get the code, update the effect, unload the effect object.
    --
    loadAndUpdateEffect :: String -> String -> IO (Either String S.GLSLEffect)
    loadAndUpdateEffect objectFile name = do
      mbStatus <- Plugins.load objectFile [] [packageDB] name
      case mbStatus of
        LoadSuccess modul (effect :: S.ShadyEffect S.Color) -> do
          Plugins.unloadAll modul
          let glslEffect = S.compileEffect ("ms" ++ show i) effect
          return (Right glslEffect)
        LoadFailure errors -> return (Left $ formatErrors errors)

filePathToUniquePrefix :: String -> String
filePathToUniquePrefix = T.unpack . go . T.pack
  where
    go = T.replace "/" "_"
       . T.replace " " "_s_"
       . T.replace "-" "_ds_"
       . T.replace "." "_dt_"

replaceSuffix :: String -> String
replaceSuffix = T.unpack . go . T.pack
  where
    go = T.replace ".hs" ".o"

--
-- Given the path to a file which contains the Shady effect code,
-- compiles it and returns @(makeStatus, name)@ where
-- @name@ is the unique name of the effect.
--
-- The @name@ is made unique by constructing it from the absolute path
-- of the file.
--
-- Preconditions: File at [path] exists and is an absolute path name.
--
makeEffect :: String -> IO (Either [String] String, String)
makeEffect path = do
  let (modulExt, modulName, name) = getNames path
  let obj = replaceSuffix path
  res <- Plugins.build path obj
    [ "-c",
      "-cpp",
      "-DmacShadyEffect=" ++ name,
      "-package-db " ++ packageDB,
      "-no-user-package-db"
    ]
  if null res
    then return $ (Right obj, name)
    else return $ (Left res, name)

  where
    getNames :: String -> (String, String, String)
    getNames path =
      let modulExt  = takeBaseName path
          modulName = modulExt
          name = filePathToUniquePrefix path
      in (modulExt, modulName, name)