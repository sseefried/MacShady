#import <Cocoa/Cocoa.h>
#import "MacShadyHooks.h"
#import "HsFFI.h"
#import "ShadyControl.h"

@interface MacShadyGLView : NSOpenGLView

@property (assign) int effectIndex;
@property          NSArray *controls;

- (id)initWithFrame:(NSRect) frame effectIndex:(int)effectIndex controls:(NSArray *)controls;

@end
