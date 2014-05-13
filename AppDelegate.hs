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
void msMouseDown(float x, float y);
void msMouseUp(float x, float y);
void msMouseDragged(float x,float y);

void msRightMouseDown(float x, float y);
void msRightMouseUp(float x, float y);
void msRightMouseDragged(float x,float y);

void msKeyDown(unsigned short keyCode, unsigned long modifierFlags);
void msKeyUp(unsigned short keyCode, unsigned long modifierFlags);

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

- (void)mouseDown:(typename NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseDown:theEvent];
  } else {
    typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
    msMouseDown(p.x, p.y);
  }
}

- (void)mouseUp:(typename NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseUp:theEvent];
  } else {

    typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                            fromView:nil];
    msMouseUp(p.x, p.y);
  }
}

- (void)mouseDragged:(typename NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseDragged:theEvent];
  } else {
    typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                            fromView:nil];
    msMouseDragged(p.x, p.y);
  }
}

- (void)rightMouseDown:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDown(p.x, p.y);
}

- (void)rightMouseUp:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseUp(p.x, p.y);
}

- (void)rightMouseDragged:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDragged(p.x, p.y);
}

- (void)keyDown:(typename NSEvent *)theEvent
{
  msKeyDown([theEvent keyCode], [theEvent modifierFlags]);
}

- (void)keyUp:(typename NSEvent *)theEvent
{
  msKeyUp([theEvent keyCode], [theEvent modifierFlags]);
}

/* This allows key down and key up events */
- (typename BOOL)acceptsFirstResponder {
  return YES;
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
