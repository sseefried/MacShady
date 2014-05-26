{-# OPTIONS_GHC -Wall #-}
module MSState where

import Data.Matrix
import           Data.Vector (Vector(..))
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as O
import           Control.Concurrent

data MouseButton = LeftMouseButton | RightMouseButton

data MSState = MSState { msMouseDownPos    :: (Float, Float)
                       , msRotationMatrix  :: Matrix Float
                       , msMouseButton     :: Maybe MouseButton
                       , msRotateVelocity  :: Vector Float
                       , msGLSLProgram     :: Maybe O.Program
                       , msZoomUniformLoc  :: Maybe O.UniformLocation
                       , msPanUniformLoc   :: Maybe O.UniformLocation
                       , msARowUniformLoc  :: Maybe O.UniformLocation
                       , msBRowUniformLoc  :: Maybe O.UniformLocation
                       , msCRowUniformLoc  :: Maybe O.UniformLocation
                       }

initialState = MSState { msMouseDownPos   = (0,0)
                       , msRotationMatrix = (identity 3)
                       , msRotateVelocity = V.fromList [0,0]
                       , msMouseButton    = Nothing
                       , msGLSLProgram    = Nothing
                       , msZoomUniformLoc = Nothing
                       , msPanUniformLoc  = Nothing
                       , msARowUniformLoc = Nothing
                       , msBRowUniformLoc = Nothing
                       , msCRowUniformLoc = Nothing
                       }

