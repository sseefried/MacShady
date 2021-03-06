{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks where

import           Foreign.StablePtr
import           Data.IORef
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Alloc (free)
import           Graphics.Rendering.OpenGL hiding (Color)
-- import           Graphics.Rendering.OpenGL.Raw
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
import           Data.Set (Set)
import qualified Data.Set as S

-- friends
import NSLog
import ShaderUtil
import MSState
import MatrixUtil
import Compile
-- The number of triangles in a row of the mesh.
-- the total mesh size is approximately the square of this number.
mESH_SIZE :: Float
mESH_SIZE = 200

vELOCITY_DRAG, vELOCITY_ACCEL :: Float
vELOCITY_DRAG = 0.03 -- value between 0 and 1. 0 is no drag. 1 is complete drag.
vELOCITY_ACCEL = 0.002

foreign export ccall msCompileAndLoadEffect :: MSEffectIndex -> CString -> IO CString
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
foreign export ccall msSetFloatUniform   :: MSEffectIndex -> CInt    -> CFloat -> IO ()


msCompileAndLoadEffect :: MSEffectIndex -> CString -> IO CString
msCompileAndLoadEffect i cstr = do
  filePath <- peekCString cstr
  res <- compileAndLoadEffect filePath i
  case res of
    Right glslEffect -> do
      initMSEffect i glslEffect
      newCString $ '0' : uiSpecOfGLSLEffect glslEffect
    Left  errors     -> do
      nsLog $ show errors
      newCString $ '1' : errors

--
-- [mesh], produces an [n] by [n] "degenerate triangle strip" that defines a mesh of
-- side length [side]. An ASCII representation of a 2x2 mesh is shown below:
--
-- The mesh spans from -side/2 to side/2 on the X and Y axes. Its centre is at the origin (0,0).
--
-- H--I--J
-- |\ |\ |
-- | \| \|
-- B--D--F
-- |\ |\ |
-- | \| \|
-- A--C--E
--
-- Each row is easily defined as a triangle strip. Four degenerate (i.e. zero area)
-- triangles are added between each row to take us to the beginning of the next row.
-- In the picture above, if A to J are points then the resulting points are:
--
-- ABCDEFFBBHDIFJ
--
-- This defines the triangles ABC BCD DEF *EFF* *FFB* *FBB* *BBH* BHD HDI DIF IFJ
-- The ones marked with asterisks are degenerate. (You can also tell they are degenerate
-- because of the repeated points.
--
mesh :: Float -> Float -> [(Float,Float)]
mesh n side =
  concat [ row y ++ degen y | y <- [-h,-h+s..h-s]]
  where
    -- s is the size of one one of the squares in the mesh.
    h = side/2
    s = side/n
    row y = concat [ [(x,y),(x, y+s)]  | x <- [-h,-h+s..h] ]
    -- adds the degenerate points required. "FB" in the example above)
    degen y = [(h,y+s), (-h,y+s)]

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
  nsLog $ "Vertex Shader\n"   ++ vertexShader glslEffect
  nsLog $ "Fragment Shader\n" ++ fragmentShader glslEffect
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


  --let a = vELOCITY_ACCEL
  --    addV  dx dy = return $ let (vx,vy) = mseRotateVelocity s
  --                        in s { mseRotateVelocity = (vx + dx, vy + dy)}
  --    addVx dx = addV dx 0
  --    addVy dy = addV 0 dy
  --case keyCode of
  --  0  {- A -} -> addVx (-a)
  --  1  {- S -} -> addVy a
  --  2  {- D -} -> addVx a
  --  13 {- W -} -> addVy (-a)
  --  _ -> return s

-- foo :: (KeyCode, Float) -> State -> State

msDraw :: MSEffectIndex -> IO ()
msDraw i = withMSEffectState i $ \s -> do
  let s' = thread [ whenKeyApplyFun (0  {- A -}, S.empty, addVx (-a))
                  , whenKeyApplyFun (1  {- S -}, S.empty, addVy   a)
                  , whenKeyApplyFun (2  {- D -}, S.empty, addVx   a)
                  , whenKeyApplyFun (13 {- W -}, S.empty, addVy (-a))
                  , whenKeyApplyFun (24 {- + -}, S.empty, zoom 0.95)
                  , whenKeyApplyFun (27 {- + -}, S.empty, zoom 1.05)
                  , applyDrag
                  , applyRotation] s

  setZoom s'
  setRotationMatrix s'
  clear [ColorBuffer, DepthBuffer]
  drawArrays TriangleStrip 0 lenMesh
  flush
  return s'
  where
    thread :: [a -> a] -> a -> a
    thread [] a = a
    thread (f:fs) a = thread fs (f a)

    a = vELOCITY_ACCEL
    addV :: Float -> Float -> MSEffectState -> MSEffectState
    addV dx dy s = let (vx,vy) = mseRotateVelocity s in s { mseRotateVelocity = (vx + dx, vy + dy) }

    addVx, addVy :: Float -> MSEffectState -> MSEffectState
    addVx dx = addV dx 0
    addVy dy = addV 0 dy

    zoom factor = modifyMSZoom (*factor)

    whenKeyApplyFun :: (KeyCode, Set KeyModifier, MSEffectState -> MSEffectState)
        -> MSEffectState -> MSEffectState
    whenKeyApplyFun (k, kms, f) s = case Map.lookup k (mseKeyMap s) of
                                      Just kms' |  kms == kms' -> f s
                                      _                        -> s

    applyDrag :: MSEffectState -> MSEffectState
    applyDrag s =
        let (vx,vy)    = mseRotateVelocity s
            d          = 1 - vELOCITY_DRAG
        in  s { mseRotateVelocity = (vx*d, vy*d) }
    -- TODO: Make rotation apply relative to camera, not object's original position.
    applyRotation :: MSEffectState -> MSEffectState
    applyRotation s =
        let msrm            = mseRotationMatrix s
            m               = msrmVal msrm
            xAxis           = xAxisOf m
            yAxis           = yAxisOf m
            (vx, vy)        = mseRotateVelocity s
        in s { mseRotationMatrix =
                 msrm { msrmVal =  rotateAboutAxis vy xAxis . rotateAboutAxis vx yAxis $ m } }

setRotationMatrix :: MSEffectState -> IO ()
setRotationMatrix s = setUniforms m aRow bRow cRow
  where
    (MSRotationMatrix m (aRow, bRow, cRow)) = mseRotationMatrix s
    elem :: Int -> Int -> M.Matrix Float -> GLfloat
    elem x y rm = fromRational . toRational $  rm M.! (x,y)
    setUniforms :: M.Matrix Float -> UniformLocation -> UniformLocation -> UniformLocation -> IO ()
    setUniforms rm aRow bRow cRow = do
      uniform aRow $= Vertex3 (elem 1 1 rm) (elem 1 2 rm) (elem 1 3 rm)
      uniform bRow $= Vertex3 (elem 2 1 rm) (elem 2 2 rm) (elem 2 3 rm)
      uniform cRow $= Vertex3 (elem 3 1 rm) (elem 3 2 rm) (elem 3 3 rm)

setZoom :: MSEffectState -> IO ()
setZoom s =
  uniform (mszUniformLoc z) $= TexCoord1 ((fromRational . toRational) (mszVal z) :: GLfloat)
  where
    z = mseZoom s

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
  let modifiers = modifiersPressed modifierFlags
--  nsLog $ "Keydown: code = " ++ show keyCode ++
--            ", modifiers = " ++ show modifiers
  return $ s { mseKeyMap = Map.insert (fromIntegral keyCode) modifiers (mseKeyMap s)}

msKeyUp :: MSEffectIndex -> CUShort -> CULong -> IO ()
msKeyUp i keyCode modifierFlags = withMSEffectState i $ \s -> do
  return $ s { mseKeyMap = Map.delete (fromIntegral keyCode) (mseKeyMap s) }

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
