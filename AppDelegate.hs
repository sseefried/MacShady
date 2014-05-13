{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends


objc_import ["<Cocoa/Cocoa.h>", "<OpenGL/gl.h>", "HsFFI.h"]

-- Hooks into Haskell-land
objc_interface [cunit|
void msDraw(void);
|]



-- Haskell code used from Objective-C.

objc_interface [cunit|

@interface MacShadyGLView : NSOpenGLView
@end

|]

objc_implementation [] [cunit|

@implementation MacShadyGLView

- (id)initWithFrame:(typename NSRect)frame
{
    NSLog(@"initWithFrame called");
    self = [super initWithFrame:frame];
    if (self) {
    }
    return self;
}

- (void)drawRect:(typename NSRect)dirtyRect
{
    [super drawRect:dirtyRect];
    msDraw();
}

@end

|]


objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate>

// IBOutlets
@property (weak, nonatomic) typename NSWindow       *window;
@property (weak, nonatomic) typename MacShadyGLView *openGLView;

@end
|]


objc_implementation [] [cunit|

@interface AppDelegate ()

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"Application did finish launching!");
}

- (typename BOOL)applicationShouldTerminateAfterLastWindowClosed:(typename NSApplication *)theApplication
{
  return YES;
}


@end
|]



objc_emit
