{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

import           Data.Map (Map)
import qualified Data.Map as M

-- friends
import MSState
import Shady.CompileEffect -- FIXME: Remove
import Shady.TestEffect -- FIXME: Remove

objc_import ["<Cocoa/Cocoa.h>", "ShadyUIGen.h"]

objc_interface [cunit|

@interface AppDelegate : NSResponder <NSApplicationDelegate>

@property typename NSWindow *window;

@end
|]

jsonForEffect :: IO String
jsonForEffect = do
  let glslEffect = compileEffect "effect_0" testEffect
  initMSEffect glslEffect
  return $ uiSpecOfGLSLEffect glslEffect

objc_implementation [Typed 'jsonForEffect ] [cunit|

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"Application did finish launching!");
  typename NSError *e = nil;
  NSLog(@"%@", jsonForEffect());
  self.window = [ShadyUIGen uiFromSpec: jsonForEffect() effectIndex: 0 error:&e];
}

@end

|]

objc_emit
