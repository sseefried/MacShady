{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks where

import           Foreign.StablePtr
import           Data.IORef
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
import State

foreign export ccall msInit              :: StablePtr (IORef State) -> IO ()
foreign export ccall msDraw              :: StablePtr (IORef State) -> IO ()
foreign export ccall msMouseDown         :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseUp           :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: StablePtr (IORef State) -> CFloat  -> CFloat -> IO ()
foreign export ccall msKeyDown           :: StablePtr (IORef State) -> CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: StablePtr (IORef State) -> CUShort -> CULong -> IO ()
foreign export ccall msResize            :: StablePtr (IORef State) -> CInt    -> CInt  -> IO ()


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
msInit :: StablePtr (IORef State) -> IO ()
msInit _ = do
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

msDraw :: StablePtr (IORef State) -> IO ()
msDraw _ = do
   nsLog $ "msDraw called"
   clear [ColorBuffer, DepthBuffer]
   drawArrays TriangleStrip 0 lenMesh
   flush

msMouseDown :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msMouseDown _ x y = do
  nsLog $ "Mouse clicked at " ++ show (x,y)

msMouseUp :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msMouseUp _ x y = nsLog $ "Mouse up at " ++ show (x,y)

msMouseDragged :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msMouseDragged _ x y = nsLog $ "Mouse dragged to " ++ show (x,y)

msRightMouseDown     :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msRightMouseDown _ x y =  nsLog $ "Right Mouse clicked at " ++ show (x,y)

msRightMouseUp       :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msRightMouseUp _ x y = nsLog $ "Right Mouse up at" ++ show (x,y)

msRightMouseDragged  :: StablePtr (IORef State) -> CFloat -> CFloat -> IO ()
msRightMouseDragged _ x y = nsLog $ "Right Mouse dragged to " ++ show (x,y)

msKeyDown :: StablePtr (IORef State) -> CUShort -> CULong -> IO ()
msKeyDown sp keyCode modifierFlags = do
  stateRef <- deRefStablePtr sp
  modifyIORef stateRef $ \(State n) -> (State (n + 1))
  state <- readIORef stateRef
  nsLog $ "The state is now " ++ show state
  nsLog $ "Key down with code = " ++ show keyCode ++
            " and modifierFlags = " ++ show modifierFlags

msKeyUp :: StablePtr (IORef State) -> CUShort -> CULong -> IO ()
msKeyUp _ keyCode modifierFlags =
  nsLog $ "Key up with code = " ++ show keyCode ++
          " and modifierFlags = " ++ show modifierFlags

msResize :: StablePtr (IORef State) -> CInt -> CInt -> IO ()
msResize sp w h = do
  nsLog $ "Resize to " ++ show (w,h)
  let s = min w h
  viewport $= (Position ((w - s)`div` 2) ((h - s) `div` 2) , Size s s )
  msDraw sp

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

