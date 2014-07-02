#import <Cocoa/Cocoa.h>
#import "HsFFI.h"

// friends
#import "MacShadyUIParser.h"
#import "MacShadyGLView.h"
#import "ShadyFloatSlider.h"

@interface MacShadyUI : NSWindow

// Path to Haskell file
@property NSString *filePath;
@property int       effectIndex;

- (id) initWithEffectFilePath:(NSString *)filePath effectIndex:(int)effectIndex;

@end