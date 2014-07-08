{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "MacShadyUI.h"]

objc_interface [cunit|

@interface AppDelegate : NSResponder <NSApplicationDelegate>

@property typename NSMutableDictionary *effects;

- (typename IBAction)trackFile:(id)sender;

@end
|]


effectFilePath :: String
effectFilePath = "/Users/sseefried/code/mac-shady-project/MacShady/examples/Flower.hs"


objc_implementation [Typed 'effectFilePath] [cunit|

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"Application did finish launching!");
  self.effects = [[NSMutableDictionary alloc] init];
  typename NSError *e = nil;
  typename MacShadyUI *shadyUI =
    [[MacShadyUI alloc] initWithEffectFilePath:effectFilePath() effectIndex:0];
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
