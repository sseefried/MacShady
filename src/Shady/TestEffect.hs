{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}
module Shady.TestEffect where


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



testImage :: Image Color
testImage = const red

testGeom :: ShadyGeometry Color
testGeom = shadyGeometry { shadyImage = testImage, shadySurface = xyPlane }

--split :: SurfD
--split (x :+ y)

testUI :: UI (ShadyGeometry Color)
testUI = do
  outerRadius <- uiSliderF "Outer" 0 0.7 1 Nothing
  innerRadius <- uiSliderF "Inner" 0 0.3 1 Nothing
  return $ testGeom { shadySurface = xyPlane }
--  return $ testGeom { shadySurface = torus (pureD outerRadius) (pureD innerRadius) }

testEffect :: ShadyEffect Color
testEffect = shadyEffect testUI