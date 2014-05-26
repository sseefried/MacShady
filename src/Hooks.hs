{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks where

import           Foreign.StablePtr
import           Data.IORef
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Foreign.Ptr (nullPtr, Ptr)
import           Foreign.Marshal.Array
import           Foreign.Storable (sizeOf)
import           Shady.CompileEffect
import           System.Exit
import           Data.Matrix (Matrix)
import qualified Data.Matrix as M
import           Data.Maybe

-- friends
import NSLog
import ShaderUtil
import MSState
import MatrixUtil

-- The number of triangles in a row of the mesh.
-- the total mesh size is approximately the square of this number.
mESH_SIZE = 200

type StableIORef a = StablePtr (IORef a)

foreign export ccall msInit              :: StableIORef MSState -> IO ()
foreign export ccall msDraw              :: StableIORef MSState -> IO ()
foreign export ccall msMouseDown         :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseUp           :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: StableIORef MSState -> CFloat  -> CFloat -> IO ()
foreign export ccall msKeyDown           :: StableIORef MSState -> CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: StableIORef MSState -> CUShort -> CULong -> IO ()
foreign export ccall msResize            :: StableIORef MSState -> CInt    -> CInt  -> IO ()


foo :: ByteString -> (Ptr (Ptr GLchar) -> IO a) -> IO a
foo bs f = BS.useAsCString bs $ \s -> withArray [s] $ \arr -> f arr

pointsToArrayBuffer :: [(Float, Float)] -> [GLfloat]
pointsToArrayBuffer = foldl f []
  where
    c = fromRational . toRational
    f rest (x,y) = c x : c y : rest

compileAndLinkEffect :: IO Program
compileAndLinkEffect  = do
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

  return p

-- called immediately after OpenGL context established
msInit :: StableIORef MSState -> IO ()
msInit sr = do
  nsLog $ "msInit called"
  vbo:_ <- genObjectNames 1 -- just generate one BufferObject
  bindBuffer ArrayBuffer $= Just vbo

  let meshData = pointsToArrayBuffer $ theMesh
  withArray meshData $ \ptr -> do
    let meshLen = fromIntegral (length meshData) * fromIntegral (sizeOf (undefined :: GLfloat))
    bufferData ArrayBuffer $= (meshLen, ptr, StaticDraw)

  p <- compileAndLinkEffect

  zoom <- get (uniformLocation p "zoom")
  pan  <- get (uniformLocation p "pan")
  aRow <- get (uniformLocation p "aRow")
  bRow <- get (uniformLocation p "bRow")
  cRow <- get (uniformLocation p "cRow")

  uniform zoom $= TexCoord1 (1 :: GLfloat)
  uniform pan  $= Vertex3 (0      :: GLfloat)      0        0
  uniform aRow $= Vertex3 (1      :: GLfloat)      0        0
  uniform bRow $= Vertex3 (0      :: GLfloat)      1        0
  uniform cRow $= Vertex3 (0      :: GLfloat)      0        1

  let attribLoc = AttribLocation 0
  vertexAttribPointer attribLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray attribLoc $= Enabled

  modifyStableIORef sr $ \s -> s { msGLSLProgram    = Just p
                                 , msZoomUniformLoc = Just zoom
                                 , msPanUniformLoc  = Just pan
                                 , msARowUniformLoc = Just aRow
                                 , msBRowUniformLoc = Just bRow
                                 , msCRowUniformLoc = Just cRow
                                 }

  -- The depth test does not work properly unless you set the depthFunc.
  -- If you don't do this you will get polygons that are occluded by others
  -- showing. It also important that a non-zero depth size is set in
  -- Cocoa's NSOpenGLView.
  depthFunc $= Just Less
  return ()

theMesh :: [(Float, Float)]
theMesh = mesh mESH_SIZE 2

lenMesh :: CInt
lenMesh = fromIntegral . length $ theMesh

msDraw :: StableIORef MSState -> IO ()
msDraw sr = do



  -- FIXME: Remove. Just a proof of concept
  s      <- readStableIORef sr
  case msGLSLProgram s of
    Just p -> do
      let rm = msRotationMatrix s
      modifyStableIORef sr $ \s -> s { msRotationMatrix = rotateYZ 0.01 rm}
      setRotationMatrix sr
    Nothing -> return ()


  clear [ColorBuffer, DepthBuffer]
  drawArrays TriangleStrip 0 lenMesh
  flush

setRotationMatrix :: StableIORef MSState -> IO ()
setRotationMatrix sr = do
  s <- readStableIORef sr
  let mbIO = do -- Maybe monad
              aRow <- msARowUniformLoc s
              bRow <- msBRowUniformLoc s
              cRow <- msCRowUniformLoc s
              return $ setUniforms (msRotationMatrix s) aRow bRow cRow
  -- nothing happens if aRow, bRow and cRow are not all "Just <something>"
  maybe (return ()) id mbIO
  where
    elem :: Int -> Int -> M.Matrix Float -> GLfloat
    elem x y rm = fromRational . toRational $  rm M.! (x,y)
    setUniforms :: M.Matrix Float -> UniformLocation -> UniformLocation -> UniformLocation -> IO ()
    setUniforms rm aRow bRow cRow = do
      uniform aRow $= Vertex3 (elem 1 1 rm) (elem 1 2 rm) (elem 1 3 rm)
      uniform bRow $= Vertex3 (elem 2 1 rm) (elem 2 2 rm) (elem 2 3 rm)
      uniform cRow $= Vertex3 (elem 3 1 rm) (elem 3 2 rm) (elem 3 3 rm)

modifyStableIORef :: StableIORef a -> (a -> a) -> IO ()
modifyStableIORef sr f = deRefStablePtr sr >>= \ioRef -> modifyIORef ioRef f

readStableIORef :: StableIORef a -> IO a
readStableIORef sr = deRefStablePtr sr >>= readIORef

coord :: CFloat -> CFloat -> (Float, Float)
coord x y = (hf x, hf y)
  where hf :: CFloat -> Float
        hf = fromRational . toRational
msMouseDown :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msMouseDown sr x y = modifyStableIORef sr
    (\s -> s { msMouseDownPos = coord x y, msMouseButton = Just LeftMouseButton })


msMouseUp :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msMouseUp sr _ _ = modifyStableIORef sr
  (\s -> s { msMouseButton = Nothing })

msMouseDragged :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msMouseDragged sr x y = return ()

msRightMouseDown     :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msRightMouseDown _ x y =  nsLog $ "Right Mouse clicked at " ++ show (x,y)

msRightMouseUp       :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msRightMouseUp _ x y = nsLog $ "Right Mouse up at" ++ show (x,y)

msRightMouseDragged  :: StableIORef MSState -> CFloat -> CFloat -> IO ()
msRightMouseDragged _ x y = nsLog $ "Right Mouse dragged to " ++ show (x,y)

msKeyDown :: StableIORef MSState -> CUShort -> CULong -> IO ()
msKeyDown sp keyCode modifierFlags = do
  nsLog $ "Key down with code = " ++ show keyCode ++
            " and modifierFlags = " ++ show modifierFlags
  --s <- readStableIORef sr
  --let rotVel = msRotationVelocity s
  --case keyCode of
  --  2 -> modifyStableIORef sr $ \s -> s { msRotationVelocity = rotVel  }

msKeyUp :: StableIORef MSState -> CUShort -> CULong -> IO ()
msKeyUp sr keyCode modifierFlags =
  nsLog $ "Key up with code = " ++ show keyCode ++
          " and modifierFlags = " ++ show modifierFlags


msResize :: StableIORef MSState -> CInt -> CInt -> IO ()
msResize sp w h = do
  nsLog $ "Resize to " ++ show (w,h)
  let s = min w h
  viewport $= (Position ((w - s)`div` 2) ((h - s) `div` 2) , Size s s )