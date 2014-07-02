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

objc_import ["<Cocoa/Cocoa.h>", "ShadyUIGen.h", "MacShadyUI.h"]

objc_interface [cunit|

@interface AppDelegate : NSResponder <NSApplicationDelegate>

@property typename NSMutableDictionary *effects;

- (typename IBAction)trackFile:(id)sender;

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
  self.effects = [[NSMutableDictionary alloc] init];
  typename NSError *e = nil;
  typename MacShadyUI *shadyUI = [ShadyUIGen uiFromSpec: jsonForEffect() effectIndex: 0 error:&e];
  [self.effects setValue:shadyUI forKey:@"effect1" ];
}

- (typename IBAction)trackFile:(id)sender {
  typename NSOpenPanel *panel = [NSOpenPanel openPanel];
  [panel beginWithCompletionHandler:^(typename NSInteger result) {
    if (result == NSFileHandlingPanelOKButton) {

      // grab a reference to what has been selected
      typename NSURL *document = [[panel URLs]objectAtIndex:0];

      // write our file name to a label
      typename NSString *url = [NSString stringWithFormat:@"%@", document];

    }
  }];
}



@end

|]

objc_emit
