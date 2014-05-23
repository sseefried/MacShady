{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module NSLog where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Foundation/Foundation.h>"]

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg] ''() [cexp| NSLog(@"%@", msg) |])

objc_emit
