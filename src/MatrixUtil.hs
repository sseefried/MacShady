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

xyRotMatrix :: Floating a => a -> Matrix a
xyRotMatrix a = M.fromList 3 3 [ cos a, -(sin a), 0
                               , sin a, cos a   , 0
                               , 0    , 0       , 1 ]


xzRotMatrix :: Floating a => a -> Matrix a
xzRotMatrix a = M.fromList 3 3 [ cos a , 0  , -(sin a)
                               , 0     , 1  , 0
                               , sin a , 0  , cos a   ]

yzRotMatrix :: Floating a => a -> Matrix a
yzRotMatrix a = M.fromList 3 3 [   1   , 0      , 0
                               ,   0   , cos a  , -(sin a)
                               ,   0   , sin a  , cos a   ]


rotateWith :: Floating a => (a -> Matrix a) -> a -> Matrix a -> Matrix a
rotateWith rotMatrix a m = m * rotMatrix a


rotateXY, rotateXZ, rotateYZ :: Floating a => a -> Matrix a -> Matrix a
rotateXY = rotateWith xyRotMatrix
rotateXZ = rotateWith xzRotMatrix
rotateYZ = rotateWith yzRotMatrix