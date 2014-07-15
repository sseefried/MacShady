{-# OPTIONS_GHC -Wall #-}
module MSState(
  module CocoaKey,
  module MSState
) where

import           Data.Matrix
import qualified Graphics.Rendering.OpenGL as O
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Shady.CompileEffect (GLSLEffect)
import           System.IO.Unsafe -- dun dun dun
import           Data.Set (Set)

--friends
import CocoaKey

--
-- The interaction between Haskell and Objective-C forces us to initialise the
-- state for a Shady effect in a step-wise manner. We must inform the
-- ShadyUIGen object of the uniform names and types for the Shady effect.
-- It is only once an OpenGL context has been opened via the Cocoa library
-- that we can compile and link the GLSL program and initialise other values.
-- Thus
--
--

type MSEffectIndex = Int

data MouseButton = LeftMouseButton | RightMouseButton

type UniformHandle a = (a, O.UniformLocation)

data MSRotationMatrix =
  MSRotationMatrix { msrmVal :: Matrix Float
                   , msrmUniformLocs :: (O.UniformLocation, O.UniformLocation, O.UniformLocation)}

data MSZoom = MSZoom { mszVal :: Float, mszUniformLoc :: O.UniformLocation }
data MSPan  = MSPan  { mspVal :: (Float, Float), mspUniformLoc :: O.UniformLocation }

data MSEffect = MSJustGLSLEffect (GLSLEffect) | MSFullEffectState MSEffectState

data MSEffectState = MSEffectState { mseMouseDownPos    :: (Float, Float)
                                   , mseMouseButton     :: Maybe MouseButton
                                   , mseRotateVelocity  :: (Float, Float)
                                   , mseGLSLProgram     :: O.Program
                                   , mseZoom            :: MSZoom
                                   , msePan             :: MSPan
                                   , mseKeyMap          :: Map KeyCode (Set KeyModifier)
                                   , mseRotationMatrix  :: MSRotationMatrix
                                   , mseGLSLEffect      :: GLSLEffect
                                   , mseUniforms        :: Map Int O.UniformLocation
                                   }

modifyMSZoom :: (Float -> Float) -> MSEffectState -> MSEffectState
modifyMSZoom f s = let z   = mseZoom s
                       val = mszVal z
                   in s { mseZoom = z { mszVal = f val}}

modifyMSPan :: ((Float,Float) -> (Float,Float)) -> MSEffectState -> MSEffectState
modifyMSPan f s = let pan = msePan s
                      val = mspVal pan
                   in s { msePan = pan { mspVal = f val}}



-- Global state
data MSState = MSState { msEffectStates :: Map MSEffectIndex MSEffect  }

-- dun dun dun!
msStateRef :: IORef (MSState)
{-# NOINLINE msStateRef #-}
msStateRef = unsafePerformIO . newIORef $ MSState M.empty

--
-- [withMSEffect effectIndex io]
--
-- [io] must return the new MSEffectState otherwise it is not updated.
--
withMSEffectState :: MSEffectIndex -> (MSEffectState -> IO MSEffectState) -> IO ()
withMSEffectState effectIndex io = do
  msState <- readIORef msStateRef
  case M.lookup effectIndex (msEffectStates msState) of
    Just (MSFullEffectState es)  -> do
      es' <- io es
      writeIORef msStateRef $
        msState { msEffectStates = M.insert effectIndex (MSFullEffectState es') (msEffectStates msState)}
    _ -> return ()


--
-- For initialising an MSEffectState. Only used in msInit in Hooks module
--
initMSEffectState :: MSEffectIndex -> (GLSLEffect -> IO MSEffectState) -> IO ()
initMSEffectState effectIndex io = do
  msState <- readIORef msStateRef
  case M.lookup effectIndex (msEffectStates msState) of
    Just (MSJustGLSLEffect effect) -> do
      es <- io effect
      writeIORef msStateRef $
        msState { msEffectStates = M.insert effectIndex (MSFullEffectState es) (msEffectStates msState)}
    _ -> return ()

initMSEffect :: MSEffectIndex -> GLSLEffect -> IO ()
initMSEffect i shadyEffect = do
  s <- readIORef msStateRef
  writeIORef msStateRef $
    s { msEffectStates = M.insert i (MSJustGLSLEffect shadyEffect) (msEffectStates s) }


initialMSEffectState :: O.Program
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> O.UniformLocation
               -> GLSLEffect
               -> Map Int O.UniformLocation
               -> MSEffectState
initialMSEffectState p zoom pan aRow bRow cRow glslEffect uniformLocMap =
  MSEffectState { mseMouseDownPos   = (0,0)
                , mseRotationMatrix = MSRotationMatrix (identity 3) (aRow, bRow, cRow)
                , mseMouseButton    = Nothing
                , mseRotateVelocity = (0,0)
                , mseGLSLProgram    = p
                , mseZoom           = MSZoom 1 zoom
                , msePan            = MSPan (0,0) pan
                , mseKeyMap         = M.empty
                , mseGLSLEffect     = glslEffect
                , mseUniforms       = uniformLocMap
                }