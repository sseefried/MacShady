{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks where

import           Foreign.C.Types
import           Graphics.Rendering.OpenGL
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Foreign.Ptr (nullPtr, Ptr)
import           Foreign.Marshal.Array
import           Foreign.Storable (sizeOf)
import           Shady.CompileEffect
import           System.Exit

-- friends
import NSLog
import ShaderUtil

foreign export ccall msInit              :: IO ()
foreign export ccall msDraw              :: IO ()
foreign export ccall msMouseDown         :: CFloat  -> CFloat -> IO ()
foreign export ccall msMouseUp           :: CFloat  -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: CFloat  -> CFloat -> IO ()
foreign export ccall msKeyDown           :: CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: CUShort -> CULong -> IO ()
foreign export ccall msResize            :: CInt    -> CInt  -> IO ()


foo :: ByteString -> (Ptr (Ptr GLchar) -> IO a) -> IO a
foo bs f = BS.useAsCString bs $ \s -> withArray [s] $ \arr -> f arr

pointsToArrayBuffer :: [(Float, Float)] -> [GLfloat]
pointsToArrayBuffer = foldl f []
  where
    c = fromRational . toRational
    f rest (x,y) = c x : c y : rest

compileAndLinkEffect :: IO ()
compileAndLinkEffect = do
  let effect = compileEffect "effect" testEffect
  vs <- loadShader VertexShader (BS.pack $ vertexShader effect) >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right vs  -> return vs
  fs <- loadShader FragmentShader (BS.pack $ fragmentShader effect) >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right fs  -> return fs

  p <- linkShaders [vs,fs] >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right p   -> return p

  currentProgram $= Just p

  attribLocation  p "meshCoords"      $= AttribLocation 0
  zoom <- get (uniformLocation p "zoom")
  pan  <- get (uniformLocation p "pan")
  aRow <- get (uniformLocation p "aRow")
  bRow <- get (uniformLocation p "bRow")
  cRow <- get (uniformLocation p "cRow")
--  mvp  <- get (uniformLocation p "gl_ModelViewProjectionMatrix")
--  norm <- get (uniformLocation p "gl_NormalMatrix")

--  uniformMat mvp $= [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
--  uniformMat norm $= [[1,0,0],[0,1,0],[0,0,1]]

  uniform zoom $= TexCoord1 (1 :: GLfloat)
  uniform pan  $= Vertex3 (0 :: GLfloat) 0 0
  uniform aRow $= Vertex3 (1 :: GLfloat) 0 0
  uniform bRow $= Vertex3 (0 :: GLfloat) 1 0
  uniform cRow $= Vertex3 (0 :: GLfloat) 0 1

-- called immediately after OpenGL context established
msInit :: IO ()
msInit = do
  nsLog $ "msInit called"

  vbo:_ <- genObjectNames 1 -- just generate one BufferObject
  bindBuffer ArrayBuffer $= Just vbo

  let meshData = pointsToArrayBuffer $ theMesh
  withArray meshData $ \ptr -> do
    let meshLen = fromIntegral (length meshData) * fromIntegral (sizeOf (undefined :: GLfloat))
    bufferData ArrayBuffer $= (meshLen, ptr, StaticDraw)

  compileAndLinkEffect

  let attribLoc = AttribLocation 0
  vertexAttribPointer attribLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray attribLoc $= Enabled
  return ()

theMesh :: [(Float, Float)]
theMesh = mesh 200 2

lenMesh :: CInt
lenMesh = fromIntegral . length $ theMesh

msDraw :: IO ()
msDraw = do
   nsLog $ "msDraw called"
   clear [ColorBuffer, DepthBuffer]
   drawArrays TriangleStrip 0 lenMesh
   flush

msMouseDown :: CFloat -> CFloat -> IO ()
msMouseDown x y = do
  nsLog $ "Mouse clicked at " ++ show (x,y)

msMouseUp :: CFloat -> CFloat -> IO ()
msMouseUp x y = nsLog $ "Mouse up at " ++ show (x,y)

msMouseDragged :: CFloat -> CFloat -> IO ()
msMouseDragged x y = nsLog $ "Mouse dragged to " ++ show (x,y)

msRightMouseDown     :: CFloat -> CFloat -> IO ()
msRightMouseDown x y =  nsLog $ "Right Mouse clicked at " ++ show (x,y)

msRightMouseUp       :: CFloat -> CFloat -> IO ()
msRightMouseUp x y = nsLog $ "Right Mouse up at" ++ show (x,y)

msRightMouseDragged  :: CFloat -> CFloat -> IO ()
msRightMouseDragged x y = nsLog $ "Right Mouse dragged to " ++ show (x,y)

msKeyDown :: CUShort -> CULong -> IO ()
msKeyDown keyCode modifierFlags =
    nsLog $ "Key down with code = " ++ show keyCode ++
            " and modifierFlags = " ++ show modifierFlags

msKeyUp :: CUShort -> CULong -> IO ()
msKeyUp keyCode modifierFlags =
  nsLog $ "Key up with code = " ++ show keyCode ++
          " and modifierFlags = " ++ show modifierFlags

msResize :: CInt -> CInt -> IO ()
msResize w h = do
  nsLog $ "Resize to " ++ show (w,h)
  let s = min w h
  viewport $= (Position ((w - s)`div` 2) ((h - s) `div` 2) , Size s s )
  msDraw

-------------------

vertexShaderSource :: ByteString
vertexShaderSource = BS.unlines
  [ "#version 120"
  , "attribute vec3 position;"
  , "void main()"
  , "{"
  , "gl_Position = vec4(position, 1);"
  , "}"
  ]

fragmentShaderSource :: ByteString
fragmentShaderSource = BS.unlines
  [ "#version 120"
  , "void main()"
  , "{"
  , "  gl_FragColor = vec4(1, 0.85, 0.35, 1);"
  , "}"
  ]


--vertexShaderSource :: ByteString
--vertexShaderSource = BS.unlines
--  [ "#version 150"
--  , "in vec3 position;"
--  , "void main()"
--  , "{"
--  , "gl_Position = vec4(position, 1);"
--  , "}"
--  ]

--fragmentShaderSource :: ByteString
--fragmentShaderSource = BS.unlines
--  [ "#version 150"
--  , "out vec4 fragmentColor;"
--  , "void main()"
--  , "{"
--  , "  fragmentColor = vec4(1, 0.85, 0.35, 1);"
--  , "}"
--  ]

