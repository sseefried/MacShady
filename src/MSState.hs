{-# OPTIONS_GHC -Wall #-}
module MSState where

import           Data.Matrix
import qualified Graphics.Rendering.OpenGL as O
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           System.IO.Unsafe -- dun dun dun


type MSEffectIndex = Int

data MouseButton = LeftMouseButton | RightMouseButton

type UniformHandle a = (a, O.UniformLocation)

data MSRotationMatrix =
  MSRotationMatrix { msrmVal :: Matrix Float
                   , msrmUniformLocs :: (O.UniformLocation, O.UniformLocation, O.UniformLocation)}

data MSZoom = MSZoom { mszVal :: Float, mszUniformLoc :: O.UniformLocation }
data MSPan  = MSPan  { mspVal :: (Float, Float), mspUniformLoc :: O.UniformLocation }


data MSEffectState = MSEffectState { mseMouseDownPos    :: (Float, Float)
                                   , mseMouseButton     :: Maybe MouseButton
                                   , mseRotateVelocity  :: (Float, Float)
                                   , mseGLSLProgram     :: O.Program
                                   , mseZoom            :: MSZoom
                                   , msePan             :: MSPan
                                   , mseRotationMatrix  :: MSRotationMatrix
                                   }

-- Global state
data MSState = MSState { msEffectIndex  :: MSEffectIndex
                       , msEffectStates :: Map MSEffectIndex MSEffectState  }

-- dun dun dun!
msStateRef :: IORef (MSState)
{-# NOINLINE msStateRef #-}
msStateRef = unsafePerformIO . newIORef $ MSState 0 M.empty


--
-- [withMSEffect effectIndex io]
--
-- [io] must return the new MSEffectState otherwise it is not updated.
--
withMSEffectState :: MSEffectIndex -> (MSEffectState -> IO MSEffectState) -> IO ()
withMSEffectState effectIndex io = do
  msState <- readIORef msStateRef
  case M.lookup effectIndex (msEffectStates msState) of
    Just es  -> do
      es' <- io es
      writeIORef msStateRef $
        msState { msEffectStates = M.insert effectIndex es' (msEffectStates msState)}
    Nothing -> return ()

-- For initialising as MSEffectState. Only used in msInit
writeMSEffectState :: MSEffectIndex -> MSEffectState -> IO ()
writeMSEffectState i es = do
  modifyIORef msStateRef $ \s -> s { msEffectStates = M.insert i es (msEffectStates s)}

--
-- [withMSState] takes a function [io] which has type [MSState -> IO ()].
-- It dereferences the stable pointer, reads from the [IORef] and if the value is
-- [Just msState] then evaluates [io msState].
-- If the value is [Nothing] it does nothing (i.e. [return ()])
--
--withMSStateRef :: MSStateRef -> (MSState -> IO ()) -> IO ()
--withMSStateRef sr io = do
--  ioRef <- deRefStablePtr sr
--  mbMSState <- readIORef ioRef
--  case mbMSState of
--    Just msState -> io msState
--    Nothing      -> return ()

--writeMSStateRef :: MSStateRef -> MSState -> IO ()
--writeMSStateRef sr v = do
--  ioRef <- deRefStablePtr sr
--  writeIORef ioRef (Just v)

----
---- [modifyMSStateRef] applies [f] to the MSState object only
---- if it is of form [Just msState]. It does nothing for [Nothing]
----
--modifyMSStateRef :: MSStateRef -> (MSState -> MSState) -> IO ()
--modifyMSStateRef sr f = do
--  ioRef     <- deRefStablePtr sr
--  mbMSState <- readIORef ioRef
--  case mbMSState of
--    Just msState -> writeIORef ioRef (Just . f $ msState)
--    Nothing      -> return ()


initialMSEffectState :: O.Program
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> MSEffectState
initialMSEffectState p zoom pan aRow bRow cRow =
  MSEffectState { mseMouseDownPos   = (0,0)
                , mseRotationMatrix = MSRotationMatrix (identity 3) (aRow, bRow, cRow)
                , mseMouseButton    = Nothing
                , mseRotateVelocity = (0,0)
                , mseGLSLProgram    = p
                , mseZoom           = MSZoom 1 zoom
                , msePan            = MSPan (0,0) pan
                }