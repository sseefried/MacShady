{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Main application module entering AppKit's application framework

module App(main, objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC
import Graphics.Rendering.OpenGL.Raw

-- friends
import NSLog (nsLog)

msDraw :: IO ()
msDraw = do
   glClearColor 1 0 0 0
   glClear gl_COLOR_BUFFER_BIT
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

foreign export ccall msDraw              :: IO ()
foreign export ccall msMouseDown         :: CFloat -> CFloat -> IO ()
foreign export ccall msMouseUp           :: CFloat -> CFloat -> IO ()
foreign export ccall msMouseDragged      :: CFloat -> CFloat -> IO ()
foreign export ccall msRightMouseDown    :: CFloat -> CFloat -> IO ()
foreign export ccall msRightMouseUp      :: CFloat -> CFloat -> IO ()
foreign export ccall msRightMouseDragged :: CFloat -> CFloat -> IO ()
foreign export ccall msKeyDown           :: CUShort -> CULong -> IO ()
foreign export ccall msKeyUp             :: CUShort -> CULong -> IO ()


--------------------------------

objc_import ["<Cocoa/Cocoa.h>"]

main :: IO ()
main = $(objc [] ''()
          [cexp| NSApplicationMain (0, NULL) |])
                   -- 'NSApplicationMain' ignores its argc and argv arguments anyway

objc_emit


