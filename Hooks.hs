module Hooks where

import Foreign.C.Types
import Graphics.Rendering.OpenGL.Raw

-- friends
import NSLog

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

msDraw :: IO ()
msDraw = do
   glClearColor 0 0 0 0
   glClear gl_COLOR_BUFFER_BIT
   glColor3f 1 0.85 0.35
   glBegin gl_TRIANGLES
   glVertex3f 0.0 0.6 0.0
   glVertex3f (-0.2) (-0.3) 0.0
   glVertex3f 0.2 (-0.3) 0.0
   glEnd
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
