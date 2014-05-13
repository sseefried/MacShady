{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Main application module entering AppKit's application framework

module App (main, objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC
import Graphics.Rendering.OpenGL.Raw

foreign export ccall msDraw      :: IO ()

msDraw :: IO ()
msDraw = do
   glClearColor 1 0 0 0
   glClear gl_COLOR_BUFFER_BIT
   glFlush


objc_import ["<Cocoa/Cocoa.h>"]


main :: IO ()
main = $(objc [] ''()
          [cexp| NSApplicationMain (0, NULL) |])
                   -- 'NSApplicationMain' ignores its argc and argv arguments anyway

objc_emit
