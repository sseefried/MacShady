{-# LANGUAGE OverloadedStrings #-}
module Hooks where

import           Data.Bits ((.|.))
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.OpenGL
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Foreign.Ptr (nullPtr, Ptr(..))
import           Foreign.Marshal.Array
import           Foreign.Storable (peek)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.C.String (withCString)

-- friends
import NSLog
import ShaderUtil

foreign export ccall msInit              :: IO ()
foreign export ccall msDraw              :: IO ()
foreign export ccall msMouseDown         :: CFloat  -> CFloat -> IO ()
foreign export ccall msMouseUp           :: CFloat  -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: CFloat  -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: CFloat  -> CFloat -> IO ()
foreign export ccall msKeyDown           :: CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: CUShort -> CULong -> IO ()
foreign export ccall msResize            :: CInt    -> CInt  -> IO ()


foo :: ByteString -> (Ptr (Ptr GLchar) -> IO a) -> IO a
foo bs f = BS.useAsCString bs $ \s -> withArray [s] $ \arr -> f arr

-- called immediately after OpenGL context established
msInit :: IO ()
msInit = do
  nsLog $ "msInit called"
  vao <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
  glBindVertexArray vao

  vbo <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
  glBindBuffer gl_ARRAY_BUFFER vbo
  withArray vertexData $ \ptr -> glBufferData gl_ARRAY_BUFFER (3 * 3 * 4) ptr gl_STATIC_DRAW

  (Right vs) <- loadShader VertexShader vertexShaderSource
  (Right fs) <- loadShader FragmentShader fragmentShaderSource
  (Right p)  <- linkShaders [vs,fs]

  attribLocation p "position" $= AttribLocation 0
  bindFragDataLocation p "fragmentColor" $= 0

  currentProgram $= Just p

  glVertexAttribPointer 0 3 gl_FLOAT 0 0 nullPtr
  glEnableVertexAttribArray 0
  return ()

drawGoldenTriangle :: IO ()
drawGoldenTriangle = do
  glColor3f 1 0.85 0.35
  glBegin gl_TRIANGLES
  glVertex3f 0.0 0.6 0.0
  glVertex3f (-0.2) (-0.3) 0.0
  glVertex3f 0.2 (-0.3) 0.0
  glEnd
  glFlush

msDraw :: IO ()
msDraw = do
   nsLog $ "msDraw called"
   glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
--   drawGoldenTriangle
   glDrawArrays gl_TRIANGLES 0 3
   glFlush

msMouseDown :: CFloat -> CFloat -> IO ()
msMouseDown x y = do
  nsLog $ "Mouse clicked at " ++ show (x,y)

msMouseUp :: CFloat -> CFloat -> IO ()
msMouseUp x y = nsLog $ "Mouse up at " ++ show (x,y)

msMouseDragged :: CFloat -> CFloat -> IO ()
msMouseDragged x y = nsLog $ "Mouse dragged to " ++ show (x,y)

msRightMouseDown     :: CFloat -> CFloat -> IO ()
msRightMouseDown x y =  nsLog $ "Right Mouse clicked at " ++ show (x,y)

msRightMouseUp       :: CFloat -> CFloat -> IO ()
msRightMouseUp x y = nsLog $ "Right Mouse up at" ++ show (x,y)

msRightMouseDragged  :: CFloat -> CFloat -> IO ()
msRightMouseDragged x y = nsLog $ "Right Mouse dragged to " ++ show (x,y)

msKeyDown :: CUShort -> CULong -> IO ()
msKeyDown keyCode modifierFlags =
    nsLog $ "Key down with code = " ++ show keyCode ++
            " and modifierFlags = " ++ show modifierFlags

msKeyUp :: CUShort -> CULong -> IO ()
msKeyUp keyCode modifierFlags =
  nsLog $ "Key up with code = " ++ show keyCode ++
          " and modifierFlags = " ++ show modifierFlags

msResize :: CInt -> CInt -> IO ()
msResize w h = do
  nsLog $ "Resize to " ++ show (w,h)
  let s = min w h
  glViewport ((w - s)`div` 2) ((h - s) `div` 2) s s
  msDraw

-------------------

vertexShaderSource :: ByteString
vertexShaderSource = BS.unlines
  [ "#version 150"
  , "in vec3 position;"
  , "void main()"
  , "{"
  , "gl_Position = vec4(position, 1);"
  , "}"
  ]

fragmentShaderSource :: ByteString
fragmentShaderSource = BS.unlines
  [ "#version 150"
  , "out vec4 fragmentColor;"
  , "void main()"
  , "{"
  , "  fragmentColor = vec4(1, 0.85, 0.35, 1);"
  , "}"
  ]

vertexData :: [GLfloat]
vertexData =
  [  0.0 ,  0.9 , 0.0
  , -0.9 , -0.9 , 0.0
  ,  0.9 , -0.9 , 0.0
  ]

