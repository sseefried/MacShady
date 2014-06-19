{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks where

import           Foreign.StablePtr
import           Data.IORef
import           Foreign.C.Types
import           Foreign.C.String
import           Graphics.Rendering.OpenGL hiding (Color)
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
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe

-- friends
import NSLog
import ShaderUtil
import MSState
import MatrixUtil

-- The number of triangles in a row of the mesh.
-- the total mesh size is approximately the square of this number.
mESH_SIZE = 200


vELOCITY_DRAG = 0.02 -- value between 0 and 1. 0 is no drag. 1 is complete drag.
vELOCITY_ACCEL = 0.01

foreign export ccall msInit              :: MSEffectIndex -> IO ()
foreign export ccall msDraw              :: MSEffectIndex -> IO ()
foreign export ccall msMouseDown         :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseUp           :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: MSEffectIndex -> CFloat  -> CFloat -> IO ()
foreign export ccall msKeyDown           :: MSEffectIndex -> CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: MSEffectIndex -> CUShort -> CULong -> IO ()
foreign export ccall msResize            :: MSEffectIndex -> CInt    -> CInt   -> IO ()
foreign export ccall msSetFloatUniform   :: MSEffectIndex -> CInt -> CFloat -> IO ()

pointsToArrayBuffer :: [(Float, Float)] -> [GLfloat]
pointsToArrayBuffer = foldl f []
  where
    c = fromRational . toRational
    f rest (x,y) = c x : c y : rest

compileAndLinkEffect :: MSEffectIndex -> GLSLEffect
                     -> IO (Program, Map Int UniformLocation)
compileAndLinkEffect i effect = do
  let uniforms = uniformNamesOfGLSLEffect effect

  vs <- loadShader VertexShader (BS.pack $ vertexShader effect) >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right vs  -> return vs
  fs <- loadShader FragmentShader (BS.pack $ fragmentShader effect) >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right fs  -> return fs

  p <- linkShaders [vs,fs] >>= \case
          Left errs -> nsLog errs >> exitFailure
          Right p   -> return p

  uniformLocs <- mapM (get . (uniformLocation p)) uniforms
  let uniformMap = foldl insertUniform Map.empty (zip [0..] uniformLocs)
      insertUniform m (i, uniformName) = Map.insert i uniformName m
  currentProgram $= Just p
  attribLocation  p "meshCoords"      $= AttribLocation 0
  return (p, uniformMap)

-- called immediately after OpenGL context established
msInit :: MSEffectIndex -> IO ()
msInit i = initMSEffectState i $ \glslEffect -> do
  nsLog $ "msInit called"
  vbo:_ <- genObjectNames 1 -- just generate one BufferObject
  bindBuffer ArrayBuffer $= Just vbo

  let meshData = pointsToArrayBuffer $ theMesh
  withArray meshData $ \ptr -> do
    let meshLen = fromIntegral (length meshData) * fromIntegral (sizeOf (undefined :: GLfloat))
    bufferData ArrayBuffer $= (meshLen, ptr, StaticDraw)

  (p, uniformMap) <- compileAndLinkEffect i glslEffect

  zoom <- get (uniformLocation p "zoom")
  pan  <- get (uniformLocation p "pan")
  aRow <- get (uniformLocation p "aRow")
  bRow <- get (uniformLocation p "bRow")
  cRow <- get (uniformLocation p "cRow")

  -- TODO: Write some helper functions to do this
  uniform zoom $= TexCoord1 (1 :: GLfloat)
  uniform pan  $= Vertex3 (0      :: GLfloat)      0        0
  uniform aRow $= Vertex3 (1      :: GLfloat)      0        0
  uniform bRow $= Vertex3 (0      :: GLfloat)      1        0
  uniform cRow $= Vertex3 (0      :: GLfloat)      0        1

  let attribLoc = AttribLocation 0
  vertexAttribPointer attribLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray attribLoc $= Enabled

  -- The depth test does not work properly unless you set the depthFunc.
  -- If you don't do this you will get polygons that are occluded by others
  -- showing. It also important that a non-zero depth size is set in
  -- Cocoa's NSOpenGLView.
  depthFunc $= Just Less
  return $ initialMSEffectState p zoom pan aRow bRow cRow glslEffect uniformMap


theMesh :: [(Float, Float)]
theMesh = mesh mESH_SIZE 2

lenMesh :: CInt
lenMesh = fromIntegral . length $ theMesh


msDraw :: MSEffectIndex -> IO ()
msDraw i = withMSEffectState i $ \s -> do
      let rotationMatrix' = applyRotation s
      setRotationMatrix rotationMatrix'
      clear [ColorBuffer, DepthBuffer]
      drawArrays TriangleStrip 0 lenMesh
      flush
      return $ s { mseRotateVelocity = applyDrag s, mseRotationMatrix = rotationMatrix'}
      where
        applyDrag s =
            let (vx,vy)    = mseRotateVelocity s
                d          = 1 - vELOCITY_DRAG
            in  (vx*d, vy*d)
        -- TODO: Make rotation apply relative to camera, not object's original position.
        applyRotation s =
            let msrm            = mseRotationMatrix s
                m               = msrmVal msrm
                xAxis           = xAxisOf m
                yAxis           = yAxisOf m
                (vx, vy)        = mseRotateVelocity s
            in msrm { msrmVal =  rotateAboutAxis vy xAxis . rotateAboutAxis vx yAxis $ m }

setRotationMatrix :: MSRotationMatrix -> IO ()
setRotationMatrix (MSRotationMatrix m (aRow, bRow, cRow)) = setUniforms m aRow bRow cRow
  where
    elem :: Int -> Int -> M.Matrix Float -> GLfloat
    elem x y rm = fromRational . toRational $  rm M.! (x,y)
    setUniforms :: M.Matrix Float -> UniformLocation -> UniformLocation -> UniformLocation -> IO ()
    setUniforms rm aRow bRow cRow = do
      uniform aRow $= Vertex3 (elem 1 1 rm) (elem 1 2 rm) (elem 1 3 rm)
      uniform bRow $= Vertex3 (elem 2 1 rm) (elem 2 2 rm) (elem 2 3 rm)
      uniform cRow $= Vertex3 (elem 3 1 rm) (elem 3 2 rm) (elem 3 3 rm)

coord :: CFloat -> CFloat -> (Float, Float)
coord x y = (hf x, hf y)
  where hf :: CFloat -> Float
        hf = fromRational . toRational

msMouseDown :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msMouseDown i x y = withMSEffectState i $ \s ->
    return $ s { mseMouseDownPos = coord x y, mseMouseButton = Just LeftMouseButton }


msMouseUp :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msMouseUp i _ _ = withMSEffectState i $ \s -> return $ s { mseMouseButton = Nothing }

msMouseDragged :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msMouseDragged _ _ _ = return ()

msRightMouseDown     :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msRightMouseDown _ x y =  nsLog $ "Right Mouse clicked at " ++ show (x,y)

msRightMouseUp       :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msRightMouseUp _ x y = nsLog $ "Right Mouse up at" ++ show (x,y)

msRightMouseDragged  :: MSEffectIndex -> CFloat -> CFloat -> IO ()
msRightMouseDragged _ x y = nsLog $ "Right Mouse dragged to " ++ show (x,y)

msKeyDown :: MSEffectIndex -> CUShort -> CULong -> IO ()
msKeyDown i keyCode modifierFlags = withMSEffectState i $ \s -> do
  nsLog $ "Key down with code = " ++ show keyCode ++
            " and modifierFlags = " ++ show modifierFlags
  let a = vELOCITY_ACCEL
      addV  dx dy = return $ let (vx,vy) = mseRotateVelocity s
                          in s { mseRotateVelocity = (vx + dx, vy + dy)}
      addVx dx = addV dx 0
      addVy dy = addV 0 dy
  case keyCode of
    0  {- A -} -> addVx (-a)
    1  {- S -} -> addVy a
    2  {- D -} -> addVx a
    13 {- W -} -> addVy (-a)
    _ -> return s

msKeyUp :: MSEffectIndex -> CUShort -> CULong -> IO ()
msKeyUp _ keyCode modifierFlags =
  nsLog $ "Key up with code = " ++ show keyCode ++
          " and modifierFlags = " ++ show modifierFlags


msResize :: MSEffectIndex -> CInt -> CInt -> IO ()
msResize _ w h = do
  nsLog $ "Resize to " ++ show (w,h)
  let s = min w h
  viewport $= (Position ((w - s)`div` 2) ((h - s) `div` 2) , Size s s )

msSetFloatUniform :: MSEffectIndex -> CInt -> CFloat -> IO ()
msSetFloatUniform i uniformIndex value = withMSEffectState i $ \s -> do
  let p = mseGLSLProgram s
  case Map.lookup (fromIntegral uniformIndex) (mseUniforms s) of
    Just loc -> uniform loc $= TexCoord1 (fromRational . toRational $ value :: GLfloat)
    Nothing -> return ()
  return s
