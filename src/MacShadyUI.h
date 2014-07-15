#import <Cocoa/Cocoa.h>
#import "HsFFI.h"

// friends
#import "MacShadyUIParser.h"
#import "MacShadyGLView.h"
#import "ShadyFloatSlider.h"

@interface MacShadyUI : NSWindow

// Path to Haskell file
@property NSString       *filePath;
@property int             effectIndex;
@property NSTextStorage  *textStorage;
@property NSScrollView   *errorLog;
@property NSTimer        *timer;
@property NSDate         *fileModificationDate;
@property NSMutableArray *controls;
@property MacShadyGLView *openGLView;

- (id) initWithEffectFilePath:(NSString *)filePath effectIndex:(int)effectIndex;

@end