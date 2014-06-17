{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC
import Foreign.StablePtr
import Data.IORef

-- friends
import MSState

--
--
--
makeMSStateRef :: IO (IORef (Maybe MSState))
makeMSStateRef = newIORef Nothing

objc_import ["<Cocoa/Cocoa.h>", "ShadyUIGen.h", "HsFFI.h"]

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate>

@property (retain) typename NSWindow *window;


@end
|]


objc_implementation ['makeMSStateRef] [cunit|

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"Application did finish launching!");
  typename HsStablePtr stateRef = makeMSStateRef();
  typename NSError *e = nil;
  self.window = [ShadyUIGen uiFromSpec:@"[ { \"sort\": \"float_slider\", \"title\": \"Spikes\", \"glslUniform\": \"spikes\", \"min\": 1, \"value\":5, \"max\": 15 }]" error:&e effectIndex: 0];
}

@end

|]



objc_emit
