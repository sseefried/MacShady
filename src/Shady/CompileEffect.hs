{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Shady.CompileEffect(
  -- data types
  ShadyEffect(..), ShadyGeometry(..), GLSLEffect {-opaque-}, UIElem {-opaque-}, UI {-opaque-},
  Color,
  -- smart constructors for UIElem
  uiTime, uiSliderF, uiSliderI, {- monad instance -}
  -- smart constructors for ShadyEffect and ShadyGeometry record types
  shadyEffect, shadyGeometry,
  -- functions that operate on opaque data type GLSLEffect
  compileEffect,                       -- constructs   GLSLEffect
  fragmentShader, vertexShader, uniformNamesOfGLSLEffect, uiSpecOfGLSLEffect,-- deconstructs GLSLEffect
  mesh, -- FIXME: move to another module
  testEffect -- FIXME: Move
) where


-- System libraries
import Text.Printf

-- friends
import Shady.Image          (Image)
import Shady.Color          (Color, HasColor, red, clear, toColor)
import Shady.CompileImage   (eyePos)
import Shady.CompileSurface (wrapSurfForEffect, EyePosE, Zoom)
import Shady.ParamSurf      (SurfD, xyPlane, torus, T)
import Shady.Lighting       (View, view1, basicStd)
import Shady.CompileE       (GLSL(..))
import Shady.CompileEs      (shaderProgram)
import Shady.Language.Exp   (R2, R3, R3E, pureE, patE, patT, ExpT(..), E(..),
                             FromE(..), HasType, pat)
import Shady.Misc           (EyePos)
import TypeUnary.Vec        (vec3, Vec1)
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import Data.List (intersperse)
import Control.Monad.State.Lazy


-- FIXME: Remove
import Data.Maclaurin

data ShadyEffect c = ShadyEffect {
  shadyGeometryUI  :: UI (ShadyGeometry c),
  shadyEyePos      :: EyePos,
  shadyViewGen     :: R3E -> View
}

-- default shadyEffect
shadyEffect :: UI (ShadyGeometry c) -> ShadyEffect c
shadyEffect shadyGeomUI = ShadyEffect {
    shadyGeometryUI  = shadyGeomUI,
    shadyEyePos      = eyePos,
    shadyViewGen     = view1 }

data ShadyGeometry c = ShadyGeometry {
  shadyImage   :: Image c,
  shadySurface :: SurfD
}

shadyGeometry :: ShadyGeometry Color
shadyGeometry = ShadyGeometry {
    shadyImage       = const $ clear,
    shadySurface     = xyPlane }

data UIElem a where
  UISliderF :: String -- ^ title
            -> Float  -- ^ lower bound
            -> Float  -- ^ default
            -> Float  -- ^ upper bound
            -> Maybe Int -- ^ ticks
            -> UIElem (E (Vec1 Float))
  UISliderI :: String -- ^ title
            -> Int    -- ^ lower bound
            -> Int    -- ^ default
            -> Int    -- ^ upper bound
            -> UIElem (E (Vec1 Int))
  UITime    :: UIElem (E (Vec1 Float))

data UIElemWithUniformIndex a = UIElemWithUniformIndex Int (UIElem a)

data UI a where
  UIElem  :: (FromE a, HasType (ExpT a)) => UIElem a -> UI a
  UIBind  :: UI a -> (a -> UI b) -> UI b
  UIReturn :: a -> UI a

data UntypedUIElem = UUISliderF String Float Float Float (Maybe Int)
                   | UUISliderI String Int Int Int
                   | UUITime

uiElemToUntyped :: UIElem a -> UntypedUIElem
uiElemToUntyped e = case e of
  (UISliderF s l d u mt)           -> UUISliderF s l d u mt
  (UISliderI s l d u)              -> UUISliderI s l d u
  UITime                           -> UUITime

elemName :: forall a.String -> UIElem a -> State Int (Int, String)
elemName uniquePrefix e = do
  i <- get
  modify (+1)
  let suffix :: String
      suffix = case e of
                 UISliderF _ _ _ _ _ -> "float_slider"
                 UISliderI _ _ _ _   -> "int_slider"
                 UITime              -> "time"
  return (i, printf "%s_%d_%s" uniquePrefix i suffix)

-- Untyped version of Variables in Shady.Language.Exp
data VU = VU { uVarName :: String, uVarType :: String }

instance Show VU where
  show vu = printf "uniform %s %s" (uVarType vu) (uVarName vu)

runUIState :: String -> UI a -> State Int (a, [(VU,String)])
runUIState uniquePrefix ui = case ui of
  UIReturn a          -> return (a, [])
  UIBind (UIElem e) f -> do
    (i,name) <- elemName uniquePrefix e
    let vu = VU name (show (patT p))
        p = pat $ name
    (a, varsAndElems) <- go . f $ (fromE . patE $ p)
    return $ (a, (vu, uiElemToJSONString i e):varsAndElems)
  UIBind nextUI f -> do
    (a, varsAndElems)  <- go nextUI
    (a', varsAndElems') <- go (f a)
    return (a', varsAndElems ++ varsAndElems')
  where
    go :: UI a -> State Int (a, [(VU, String)])
    go = runUIState uniquePrefix

runUI :: String -> UI a -> (a, [(VU, String)])
runUI uniquePrefix = fst . flip runState 0 . runUIState uniquePrefix

instance Monad UI where
  return = UIReturn
  (>>=)  = UIBind

type VertexPosAttribute = R2

data GLSLEffect = GLSLEffect (GLSL ((), (R3, (R3, (R3, (R3, Zoom)))))
                               VertexPosAttribute) [VU] [String]

toShader :: [VU] -> String -> String
toShader uniforms shader = printf "%s\n%s\n%s" shaderHeaders uniformDecs shader
  where
    uniformDecs :: String
    uniformDecs = concatMap (printf "%s;\n" . show) $ uniforms
    shaderHeaders = unlines [
        "#version 120"
--      , "precision highp float;"
      , ""
      , "#define _attribute meshCoords"
      , "#define _uniform_SSSSS zoom"
      , "#define _uniform_SSSSF pan"
      , "/* aRow, bRow, and cRow are three rows of the rotation matrix */"
      , "#define _uniform_SSSF  cRow"
      , "#define _uniform_SSF   bRow"
      , "#define _uniform_SF    aRow"
      , "/* varying_F is just copy of mesh_coords"
      , "   varying_S is vertex position of mesh coordinate after transformation */"
      ]


--
-- | A selector function for extracting fragment shader from GLSLEffect
--
fragmentShader  :: GLSLEffect -> String
fragmentShader (GLSLEffect (GLSL _ fs _ _) uniforms _) = toShader uniforms fs

--
-- | A selector function for extracting vertex shader from GLSLEffect
--
vertexShader :: GLSLEffect -> String
vertexShader (GLSLEffect (GLSL vs _ _ _) uniforms _) = toShader uniforms vs

uniformNamesOfGLSLEffect :: GLSLEffect -> [String]
uniformNamesOfGLSLEffect (GLSLEffect _ uniforms _) = map uVarName uniforms

-- Produces UI Specification in JSON format
uiSpecOfGLSLEffect :: GLSLEffect -> String
uiSpecOfGLSLEffect (GLSLEffect _ _ jsonStrings) = "[" ++ (concat $ (intersperse ", " jsonStrings)) ++ "]"

--
-- | Compiles a ShadyEffect to a GLSL program.
--
-- An OpenGL (or WebGL) program that links to this GLSL program
-- should set up a 'uniform' value denoting the time value of the animation
-- and an 'attribute' denoting the vertex positions.
--
-- (For definitions of 'uniform' and 'attribute' see the GLSL spec)
--
compileEffect :: forall c. (HasColor c) => String -> ShadyEffect c -> GLSLEffect
compileEffect prefix e = GLSLEffect glsl uniforms jsons
  where
    glsl = shaderProgram $ wrapSurfForEffect eyePosE (\() -> fullSurf)
    (uniforms,jsons) = unzip uniformsAndJsons
    (geom, uniformsAndJsons) = runUI prefix $ shadyGeometryUI e
    fullSurf = (basicStd, shadyViewGen e, surface, image)
       where
         surface = shadySurface geom
         image   = toColor . shadyImage geom
    eyePosE :: EyePosE
    eyePosE = pureE (vec3 ex ey ez) where (ex,ey,ez) = shadyEyePos e

--
-- Smart constructors
--
uiTime :: UI (E (Vec1 Float))
uiTime    = UIElem UITime

--
-- | Creates a slider that produces Float values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderF :: String -> Float -> Float -> Float -> Maybe Int -> UI (E (Vec1 Float))
uiSliderF title minVal defaultVal maxVal mbTicks =
  UIElem (UISliderF title minVal' defaultVal' maxVal' mbTicks)
  where (minVal', defaultVal', maxVal') = sensible (minVal, defaultVal, maxVal)

--
-- | Creates a slider that produces Int values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderI :: String -> Int -> Int -> Int -> UI (E (Vec1 Int))
uiSliderI title minVal defaultVal maxVal =
  UIElem (UISliderI title minVal' defaultVal' maxVal')
  where
    (minVal', defaultVal', maxVal') = sensible (minVal, defaultVal, maxVal)

--
-- A helper function to clamp "bad" slider values to sensible ones.
--
-- Examples:
--  sensible (0, 2, -3)  == (0, 0, 0)
--  sensible (0, 5,  3)  == (0, 3, 3)
--  sensible (0, -5, 3)  == (0, 0, 3)
--  sensible (0, 2,  5)  == (0, 2, 5)
--  sensible (0, 2,  5)  == (0, 2, 5)
--
sensible :: (Num a, Ord a) => (a,a,a) -> (a,a,a)
sensible (minVal, defaultVal, maxVal) = (minVal, defaultVal', maxVal')
  where
    maxVal'     = minVal `max` maxVal
    defaultVal' = (minVal `max` defaultVal) `min` maxVal'


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


---------------------------------

instance ToJSON (UIElemWithUniformIndex a) where
  toJSON (UIElemWithUniformIndex uniformIndex uiElem) = case uiElem of
    UISliderF title min value max mbTicks ->
      object ([ "sort" .= ("float_slider" :: String), "glslUniformIndex" .= uniformIndex,
                "title" .= title, "min" .= min,
                "value" .= value, "max" .= max ] ++ maybe [] (\t -> ["ticks" .= t]) mbTicks)
    UISliderI title min value max ->
      object [ "sort" .= ("int_slider" :: String), "glslUniformIndex" .= uniformIndex, "title" .= title,
               "min" .= min, "value" .= value, "max" .= max ]
    UITime -> object [ "sort" .= ("time" :: String)]

uiElemToJSONString :: Int -> UIElem a -> String
uiElemToJSONString uniformIndex uiElem =
  BS.unpack . JSON.encode $ UIElemWithUniformIndex uniformIndex uiElem



--------------------------------------------------------


testSurf :: T -> T -> SurfD
testSurf outerRadius innerRadius = torus 0.7 0.3 -- outerRadius 0.3 -- innerRadius

testImage :: Image Color
testImage = const red

testGeom :: T -> T -> ShadyGeometry Color
testGeom o i = shadyGeometry { shadyImage = testImage, shadySurface = testSurf o i }

testUI :: UI (ShadyGeometry Color)
testUI = do
  outerRadius <- uiSliderF "Outer" 0 0.7 1 Nothing
  innerRadius <- uiSliderF "Inner" 0 0.3 1 Nothing
  return $ testGeom (pureD outerRadius) (pureD innerRadius)

testEffect :: ShadyEffect Color
testEffect = shadyEffect testUI