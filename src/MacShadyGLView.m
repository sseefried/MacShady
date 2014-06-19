#include "MacShadyGLView.h"

static const NSTimeInterval  kScheduledTimerInSeconds      = 1.0f/60.0f;
@implementation MacShadyGLView

BOOL initialised = NO;
NSTimer            *timer;            // timer to update the view content


/*
 * In order for OpenGL's "depth test" to work you have to have a non-zero depth size
 * in pixel format. This can be set in Interface Builder but I have opted to enable it
 * programatically.
 */
- (id)initWithFrame:(NSRect)frame effectIndex:(int)effectIndex
{
  self = [super initWithFrame: frame];
  if (self) {
    NSOpenGLPixelFormatAttribute attrs[] =
      {
          NSOpenGLPFADoubleBuffer,
          NSOpenGLPFADepthSize, 8,
          0
      };

    NSOpenGLPixelFormat *pf = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];

    if (!pf)
    {
        NSLog(@"No OpenGL pixel format");
    }

    NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pf shareContext:nil];

    self.effectIndex = effectIndex;
    [self setPixelFormat:pf];
    [self setOpenGLContext:context];
  }

  return self;
}

/*
 * In order to get GLSL shaders version 1.5 to display you must enable OpenGL "3.2 Core Profile".
 *
 * We are NOT currently using verison 1.5 but 1.2 instead. Setting the 3.2 Core Profile will
 * mean this doesn't work.
 */

- (void)coreProfile32
{
  NSOpenGLPixelFormatAttribute attrs[] =
    {
        // Must specify the 3.2 Core Profile to use OpenGL 3.2
        NSOpenGLPFAOpenGLProfile,
        NSOpenGLProfileVersion3_2Core,
        0
    };

  NSOpenGLPixelFormat *pf = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];

  if (!pf)
  {
      NSLog(@"No OpenGL pixel format");
  }

  NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pf shareContext:nil];

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
    [self initUpdateTimer];
    msInit(self.effectIndex);
}

- (void)drawRect:(NSRect)dirtyRect
{
  [super drawRect:dirtyRect];
  if (!initialised) {
    initialised = YES;
    [self initialise];
  }
  msDraw(self.effectIndex);
  [[self openGLContext] flushBuffer];
}

- (void)mouseDown:(NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseDown:theEvent];
  } else {
    NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
    msMouseDown(self.effectIndex, p.x, p.y);
  }
}

- (void)mouseUp:(NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseUp:theEvent];
  } else {

    NSPoint p = [self convertPoint:[theEvent locationInWindow]
                            fromView:nil];
    msMouseUp(self.effectIndex, p.x, p.y);
  }
}

- (void)mouseDragged:(NSEvent *)theEvent
{
  if( [theEvent modifierFlags] & NSRightMouseDown )
  {
    [self rightMouseDragged:theEvent];
  } else {
    NSPoint p = [self convertPoint:[theEvent locationInWindow]
                            fromView:nil];
    msMouseDragged(self.effectIndex, p.x, p.y);
  }
}

- (void)rightMouseDown:(NSEvent *)theEvent
{
  NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDown(self.effectIndex, p.x, p.y);
}

- (void)rightMouseUp:(NSEvent *)theEvent
{
  NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseUp(self.effectIndex, p.x, p.y);
}

- (void)rightMouseDragged:(NSEvent *)theEvent
{
  NSPoint p = [self convertPoint:[theEvent locationInWindow]
                          fromView:nil];
  msRightMouseDragged(self.effectIndex, p.x, p.y);
}

- (void)keyDown:(NSEvent *)theEvent
{
  msKeyDown(self.effectIndex, [theEvent keyCode], [theEvent modifierFlags]);
}

- (void)keyUp:(NSEvent *)theEvent
{
  msKeyUp(self.effectIndex, [theEvent keyCode], [theEvent modifierFlags]);
}

- (void)reshape
{
  [[self openGLContext] update];
  NSRect bounds = [self bounds];
  msResize(self.effectIndex, bounds.size.width, bounds.size.height);

}
/* This allows key down and key up events */
- (BOOL)acceptsFirstResponder {
  return YES;
}

@end
