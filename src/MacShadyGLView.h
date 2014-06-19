#import <Cocoa/Cocoa.h>
#import "MacShadyHooks.h"
#import "HsFFI.h"

@interface MacShadyGLView : NSOpenGLView

@property (assign) int effectIndex;

- (id)initWithFrame:(NSRect) frame effectIndex:(int)effectIndex;

@end
