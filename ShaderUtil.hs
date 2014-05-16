module ShaderUtil where

import Graphics.Rendering.OpenGL
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

type GLSLLog = String

loadShader :: ShaderType -> ByteString -> IO (Either GLSLLog Shader)
loadShader st src = do
  shader <- createShader st
  shaderSourceBS shader $= src
  compileShader shader
  ok <- get (compileStatus shader)
  case ok of
    True -> return $ Right shader
    False -> do
      infoLog <- get (shaderInfoLog shader)
      deleteObjectNames [shader]
      return $ Left infoLog

linkShaders :: [Shader] -> IO (Either GLSLLog Program)
linkShaders shaders = do
  p <- createProgram
  attachedShaders p $= shaders
  linkProgram p
  ok <- get (linkStatus p)
  case ok of
    True  -> return $ Right p
    False -> do
      deleteObjectNames [p]
      infoLog <- get (programInfoLog p)
      return $ Left infoLog

