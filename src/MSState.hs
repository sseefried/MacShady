{-# OPTIONS_GHC -Wall #-}
module MSState where

import           Data.Matrix
import qualified Graphics.Rendering.OpenGL as O
import           Foreign.StablePtr
import           Data.IORef


type MSStateRef = StablePtr (IORef (Maybe MSState))

data MouseButton = LeftMouseButton | RightMouseButton

type UniformHandle a = (a, O.UniformLocation)

data MSRotationMatrix =
  MSRotationMatrix { msrmVal :: Matrix Float
                   , msrmUniformLocs :: (O.UniformLocation, O.UniformLocation, O.UniformLocation)}

data MSZoom = MSZoom { mszVal :: Float, mszUniformLoc :: O.UniformLocation }
data MSPan  = MSPan  { mspVal :: (Float, Float), mspUniformLoc :: O.UniformLocation }


data MSState = MSState { msMouseDownPos    :: (Float, Float)
                       , msMouseButton     :: Maybe MouseButton
                       , msRotateVelocity  :: (Float, Float)
                       , msGLSLProgram     :: O.Program
                       , msZoom            :: MSZoom
                       , msPan             :: MSPan
                       , msRotationMatrix  :: MSRotationMatrix
                       }

--
-- [withMSState] takes a function [io] which has type [MSState -> IO ()].
-- It dereferences the stable pointer, reads from the [IORef] and if the value is
-- [Just msState] then evaluates [io msState].
-- If the value is [Nothing] it does nothing (i.e. [return ()])
--
withMSStateRef :: MSStateRef -> (MSState -> IO ()) -> IO ()
withMSStateRef sr io = do
  ioRef <- deRefStablePtr sr
  mbMSState <- readIORef ioRef
  case mbMSState of
    Just msState -> io msState
    Nothing      -> return ()

writeMSStateRef :: MSStateRef -> MSState -> IO ()
writeMSStateRef sr v = do
  ioRef <- deRefStablePtr sr
  writeIORef ioRef (Just v)

--
-- [modifyMSStateRef] applies [f] to the MSState object only
-- if it is of form [Just msState]. It does nothing for [Nothing]
--
modifyMSStateRef :: MSStateRef -> (MSState -> MSState) -> IO ()
modifyMSStateRef sr f = do
  ioRef     <- deRefStablePtr sr
  mbMSState <- readIORef ioRef
  case mbMSState of
    Just msState -> writeIORef ioRef (Just . f $ msState)
    Nothing      -> return ()


initialMSState :: O.Program
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> MSState
initialMSState p zoom pan aRow bRow cRow =
  MSState { msMouseDownPos   = (0,0)
          , msRotationMatrix = MSRotationMatrix (identity 3) (aRow, bRow, cRow)
          , msMouseButton    = Nothing
          , msRotateVelocity = (0,0)
          , msGLSLProgram    = p
          , msZoom           = MSZoom 1 zoom
          , msPan            = MSPan (0,0) pan
          }