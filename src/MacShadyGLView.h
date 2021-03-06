#import <Cocoa/Cocoa.h>
#import "MacShadyHooks.h"
#import "HsFFI.h"
#import "ShadyControl.h"

@interface MacShadyGLView : NSOpenGLView

@property (assign) int effectIndex;
@property          NSArray *controls;
@property          NSTimer *timer;            // timer to update the view content
@property          BOOL     initialised;



- (id)initWithFrame:(NSRect) frame effectIndex:(int)effectIndex controls:(NSArray *)controls;

@end
