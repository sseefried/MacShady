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
makeStateRef :: IO (IORef MSState)
makeStateRef = do
  newIORef initialState


objc_import ["<Cocoa/Cocoa.h>", "<OpenGL/gl.h>", "HsFFI.h"]

-- Hooks into Haskell-land
objc_interface [cunit|

void msInit(typename HsStablePtr stateRef);

void msDraw(typename HsStablePtr stateRef);
void msMouseDown(typename HsStablePtr stateRef,float x, float y);
void msMouseUp(typename HsStablePtr stateRef, float x, float y);
void msMouseDragged(typename HsStablePtr stateRef, float x,float y);

void msRightMouseDown(typename HsStablePtr stateRef, float x, float y);
void msRightMouseUp(typename HsStablePtr stateRef, float x, float y);
void msRightMouseDragged(typename HsStablePtr stateRef, float x,float y);

void msKeyDown(typename HsStablePtr stateRef, unsigned short keyCode, unsigned long modifierFlags);
void msKeyUp(typename HsStablePtr stateRef, unsigned short keyCode, unsigned long modifierFlags);
void msResize(typename HsStablePtr stateRef, unsigned int width, unsigned int height);

|]



-- Haskell code used from Objective-C.

objc_interface [cunit|

@interface MacShadyGLView : NSOpenGLView

@property (assign) typename HsStablePtr stateRef;

@end

|]

objc_implementation ['makeStateRef] [cunit|

static const typename NSTimeInterval  kScheduledTimerInSeconds      = 1.0f/60.0f;
@implementation MacShadyGLView

typename BOOL initialised = NO;
typename NSTimer            *timer;            // timer to update the view content


/*
 * In order for OpenGL's "depth test" to work you have to have a non-zero depth size
 * in pixel format. This can be set in Interface Builder but I have opted to enable it
 * programatically.
 */
- (void)awakeFromNib
{
  NSLog(@"awakeFromNIB called");
  typename NSOpenGLPixelFormatAttribute attrs[] =
    {
        NSOpenGLPFADoubleBuffer,
        NSOpenGLPFADepthSize, 8,
        0
    };

  typename NSOpenGLPixelFormat *pf = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];

  if (!pf)
  {
      NSLog(@"No OpenGL pixel format");
  }

  typename NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pf shareContext:nil];

  [self setPixelFormat:pf];
  [self setOpenGLContext:context];
}

/*
 * In order to get GLSL shaders version 1.5 to display you must enable OpenGL "3.2 Core Profile".
 *
 * We are NOT currently using verison 1.5 but 1.2 instead. Setting the 3.2 Core Profile will
 * mean this doesn't work.
 */

- (void)coreProfile32
{
  typename NSOpenGLPixelFormatAttribute attrs[] =
    {
        // Must specify the 3.2 Core Profile to use OpenGL 3.2
        NSOpenGLPFAOpenGLProfile,
        NSOpenGLProfileVersion3_2Core,
        0
    };

  typename NSOpenGLPixelFormat *pf = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];

  if (!pf)
  {
      NSLog(@"No OpenGL pixel format");
  }

  typename NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pf shareContext:nil];

  [self setPixelFormat:pf];
  [self setOpenGLContext:context];
}

/*
 * Note on the used of the 'intialised' variable.
 *
 * When initialising an object using a .nib file
 * one can only guarantee that all initialisation has been
 * done at the point where 'drawRect' is called for the first time.
 * This forces us to do the rather ugly trick below where we call
 * 'msInit' only the first time through.
 *
 */

- (void)redraw
{
  [self drawRect:[self bounds]];
}

- (void)initUpdateTimer
{
  timer = [NSTimer timerWithTimeInterval:kScheduledTimerInSeconds
                target:self
                selector:@selector(redraw)
                userInfo:nil
                 repeats:YES];

  [[NSRunLoop currentRunLoop] addTimer:timer
               forMode:NSDefaultRunLoopMode];

  [[NSRunLoop currentRunLoop] addTimer:timer
               forMode:NSEventTrackingRunLoopMode];

}

- (void)initialise{
    self.stateRef = makeStateRef();
    [self initUpdateTimer];
    msInit(self.stateRef);
}

- (void)drawRect:(typename NSRect)dirtyRect
{
  [super drawRect:dirtyRect];
  if (!initialised) {
    initialised = YES;
    [self initialise];
  }
  msDraw(self.stateRef);
  [[self openGLContext] flushBuffer];
}

- (void)mouseDown:(typename NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseDown:theEvent];
  } else {
    typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
    msMouseDown(self.stateRef, p.x, p.y);
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
    msMouseUp(self.stateRef, p.x, p.y);
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
    msMouseDragged(self.stateRef, p.x, p.y);
  }
}

- (void)rightMouseDown:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDown(self.stateRef, p.x, p.y);
}

- (void)rightMouseUp:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseUp(self.stateRef, p.x, p.y);
}

- (void)rightMouseDragged:(typename NSEvent *)theEvent
{
  typename NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDragged(self.stateRef, p.x, p.y);
}

- (void)keyDown:(typename NSEvent *)theEvent
{
  msKeyDown(self.stateRef, [theEvent keyCode], [theEvent modifierFlags]);
}

- (void)keyUp:(typename NSEvent *)theEvent
{
  msKeyUp(self.stateRef, [theEvent keyCode], [theEvent modifierFlags]);
}

- (void)reshape
{
  [[self openGLContext] update];
  typename NSRect bounds = [self bounds];
  msResize(self.stateRef, bounds.size.width, bounds.size.height);

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
