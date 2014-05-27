{-# OPTIONS_GHC -Wall #-}
module MatrixUtil where

--import           Data.Vector (Vector)
--import qualified Data.Vector as V

import           Data.Matrix (Matrix)
import qualified Data.Matrix as M

--
-- The functions in this module are not as type-safe as I would like.
-- [rotateInPlane] takes a vector of length 3 and a 3x3 matrix and rotates about that plane
-- by an angle. But there is nothing stopping you from passing in a vector or matrix
-- of the wrong dimensions.
--

rotateAboutAxisMatrix :: Float -> (Float, Float, Float) -> Matrix Float
rotateAboutAxisMatrix ang (vx,vy,vz) =
  M.fromList 3 3 [ ux*ux*t + c,    uy*ux*t + uz*s, uz*ux*t - uy*s,
                   ux*uy*t - uz*s, uy*uy*t + c,    uz*uy*t + ux*s,
                   ux*uz*t + uy*s, uy*uz*t - ux*s, uz*uz*t + c    ]
  where
    d = sqrt (vx*vx + vy*vy + vz*vz)
    (ux,uy,uz) = (vx/d, vy/d, vz/d)
    s = sin ang
    c = cos ang
    t = 1 - c

rotateAboutAxis :: Float -> (Float, Float, Float) -> Matrix Float -> Matrix Float
rotateAboutAxis ang axis m = m * rotateAboutAxisMatrix ang axis

xAxisOf m = (m M.! (1,1), m M.! (1,2), m M.! (1,3))
yAxisOf m = (m M.! (2,1), m M.! (2,2), m M.! (2,3))