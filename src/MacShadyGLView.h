#import "HsFFI.h"
#import <Cocoa/Cocoa.h>

void msInit(HsStablePtr stateRef);

void msDraw(HsStablePtr stateRef);
void msMouseDown(HsStablePtr stateRef,float x, float y);
void msMouseUp(HsStablePtr stateRef, float x, float y);
void msMouseDragged(HsStablePtr stateRef, float x,float y);

void msRightMouseDown(HsStablePtr stateRef, float x, float y);
void msRightMouseUp(HsStablePtr stateRef, float x, float y);
void msRightMouseDragged(HsStablePtr stateRef, float x,float y);

void msKeyDown(HsStablePtr stateRef, unsigned short keyCode, unsigned long modifierFlags);
void msKeyUp(HsStablePtr stateRef, unsigned short keyCode, unsigned long modifierFlags);
void msResize(HsStablePtr stateRef, unsigned int width, unsigned int height);
void msSetFloatUniform(HsStablePtr stateRef, char *uniformName, float value);

@interface MacShadyGLView : NSOpenGLView

@property (assign) HsStablePtr stateRef;

- (id)initWithFrame:(NSRect) frame stateRef:(HsStablePtr)stateRef;

@end
