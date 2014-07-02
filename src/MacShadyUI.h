#import <Cocoa/Cocoa.h>
#import "HsFFI.h"

// friends
#import "MacShadyGLView.h"
#import "ShadyFloatSlider.h"

@interface MacShadyUI : NSWindow

// Path to Haskell file
@property NSString *filePath;
@property int       effectIndex;

@property NSMutableArray *controls; // FIXME: Remove if not required

- (id) initWithUISpec:(NSArray *)uiSpec effectIndex:(int)effectIndex;

@end