{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Flower (macShadyEffect) where

import Data.MemoTrie
import Data.Basis
import Data.Boolean
import Shady.Lighting
import Data.Derivative
import Data.VectorSpace
import Shady.ParamSurf
import Shady.CompileImage
import Shady.CompileSurface
import Shady.Color
import Shady.Language.Exp
import Shady.Complex
import Shady.Misc
import Shady.CompileE
import Shady.Image
import qualified TypeUnary.Vec as V
import Shady.CompileEffect
import Control.Applicative hiding ((<*))

import Shady.Language.Operator (Op(..))

instance (HasTrie (Basis a), AdditiveGroup b, HasBasis a, IfE b) => IfE (a :> b) where
  ifE b e1 e2 = liftD2 (ifE b) e1 e2

instance IfE Color where
  ifE b c1 c2 = r4ToColor $ ifE b (colorToR4 c1) (colorToR4 c2)

col (x :+ y) = ifE (x <* 0.3) (ifE (x <* 0.1) red (rgba 0 0 0 1)) red

--------------

testImage :: Image Color
testImage = const red

testGeom :: ShadyGeometry Color
testGeom = shadyGeometry { shadyImage = testImage, shadySurface = xyPlane }

scaleIt :: T -> SurfD -> SurfD
scaleIt f s p = let (x,y,z) = s p in (f * x, f * y, f * z)

panIt :: (T,T,T) -> SurfD -> SurfD
panIt (dx,dy,dz) s p = let (x,y,z) = s p in (x + dx, y + dy, z + dz)

noUI :: SurfD -> UI (ShadyGeometry Color)
noUI s = return $ testGeom { shadySurface = s }

xyPlaneUI = noUI $ xyPlane

torusUI :: UI (ShadyGeometry Color)
torusUI = do
  inner <- uiSliderF "Inner" 0 0.3 1 Nothing
  outer <- uiSliderF "Outer" 0 0.7 1 Nothing
  return $ testGeom { shadySurface = torus (pureD outer) (pureD inner) }
--  return $ testGeom { shadySurface = discont' (uscale2 0.2 sphere1) (xyPlane) }

--  return $ testGeom { shadySurface = displace (discont a b) eggcrateH}
--  return $ testGeom { shadySurface = torus (pureD outerRadius) (pureD innerRadius) }

flowerUI :: UI (ShadyGeometry Color)
flowerUI = do
  inner   <- uiSliderF "Inner" 0 0.3 1 Nothing
  outer   <- uiSliderF "Outer" 0 0.7 1 Nothing
  periods <- uiSliderF "Periods" 2 3 10 (Just 8)
  height  <- uiSliderF "Spike height" 0 0 2 Nothing
  let h = pureD height
      variableEggcrate = (h*) . eggcrateH . uscale2 (pureD periods)
      variableTorus = torus (pureD outer) (pureD inner)
      flower = displace variableTorus variableEggcrate
  return $ testGeom { shadyImage = const (rgba 1 0.5 0.5 1), shadySurface = flower }

revolutionUI = noUI $ revolve $ \x -> (x :+ x*x)

macShadyEffect :: ShadyEffect Color
macShadyEffect = shadyEffect $ flowerUI