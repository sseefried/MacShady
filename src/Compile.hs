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

-- friends
import qualified Shady.CompileEffect as S

-- | Compile the effect code. Return either the @Effect@ or a compiler error (Left).
--
compileEffect :: FilePath -> IO (Either String S.GLSLEffect)
compileEffect path = do
  (res, name) <- makeEffect path
  case res of
     MakeSuccess _  objectFile -> loadAndUpdateEffect objectFile name
     MakeFailure errors        -> return (Left $ formatErrors errors)
  where
    formatErrors errors = concat $ intersperse "\n" errors

    --
    -- Load the plugin, get the code, update the effect, unload the effect object.
    --
    loadAndUpdateEffect :: String -> String -> IO (Either String S.GLSLEffect)
    loadAndUpdateEffect objectFile name = do
      mbStatus <- Plugins.load objectFile [] [] name
      case mbStatus of
        LoadSuccess modul (effect :: S.ShadyEffect S.Color) -> do
          Plugins.unload modul
          let glslEffect = S.compileEffect name effect
          return (Right glslEffect)
        LoadFailure errors -> return (Left $ formatErrors errors)

filePathToUniquePrefix :: String -> String
filePathToUniquePrefix = T.unpack . go . T.pack
  where
    go = T.replace "/" "_"

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
makeEffect :: String -> IO (MakeStatus, String)
makeEffect path = do
  let (modulExt, modulName, name) = getNames path
  res <- Plugins.make path [ "-DMODULE_NAME=" ++ modulName
                           , "-DEFFECT_NAME=" ++ name ]

  return (res, name)
  where
    getNames :: String -> (String, String, String)
    getNames path =
      let modulExt  = takeBaseName path
          modulName = modulExt
          name = filePathToUniquePrefix path
      in (modulExt, modulName, name)