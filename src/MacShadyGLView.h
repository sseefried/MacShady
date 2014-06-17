#import "HsFFI.h"
#import <Cocoa/Cocoa.h>

void msInit(int effectIndex);

void msDraw(int effectIndex);
void msMouseDown(int effectIndex,float x, float y);
void msMouseUp(int effectIndex, float x, float y);
void msMouseDragged(int effectIndex, float x,float y);

void msRightMouseDown(int effectIndex, float x, float y);
void msRightMouseUp(int effectIndex, float x, float y);
void msRightMouseDragged(int effectIndex, float x,float y);

void msKeyDown(int effectIndex, unsigned short keyCode, unsigned long modifierFlags);
void msKeyUp(int effectIndex, unsigned short keyCode, unsigned long modifierFlags);
void msResize(int effectIndex, unsigned int width, unsigned int height);
void msSetFloatUniform(int effectIndex, char *uniformName, float value);

@interface MacShadyGLView : NSOpenGLView

@property (assign) int effectIndex;

- (id)initWithFrame:(NSRect) frame effectIndex:(int)effectIndex;

@end
