#import "MacShadyUI.h"

@implementation MacShadyUI


+ (NSControl *)controlFromUIElem:(NSDictionary *)uiElement effectIndex:(int)effectIndex
{
  NSString *sort = [uiElement valueForKey:@"sort"];
  NSControl *control = nil;
  if ([sort isEqualToString:@"float_slider"]) {
    NSNumber *uniformIndex =  [uiElement valueForKey:@"glslUniformIndex"];
    NSNumber *minValue = [uiElement valueForKey:@"min"];
    NSNumber *value    = [uiElement valueForKey:@"value"];
    NSNumber *maxValue = [uiElement valueForKey:@"max"];
    NSNumber *ticks    = [uiElement valueForKey:@"ticks"];
    NSString *title    = [uiElement valueForKey:@"title"];

    if (ticks) {
      control = (NSControl *)[[ShadyFloatSlider alloc]
                    initWithUniformIndex:[uniformIndex intValue] effectIndex: effectIndex title:title
                               minValue:[minValue doubleValue] value:[value doubleValue]
                               maxValue:[maxValue doubleValue] ticks:[ticks doubleValue]];
    } else {
      control = (NSControl *)[[ShadyFloatSlider alloc] initWithUniformIndex:[uniformIndex intValue]
                   effectIndex: effectIndex title:title
                      minValue:[minValue doubleValue] value:[value doubleValue]
                      maxValue:[maxValue doubleValue]];
    }
  }
  return control;
}

- (id) initWithEffectFilePath:(NSString *)filePath effectIndex:(int)effectIndex {
  CGFloat width = 800, height = 600;
  NSRect frame = NSMakeRect(0, 0, width, height);

  self  = [super initWithContentRect:frame
                           styleMask:(NSTitledWindowMask |
                                      NSClosableWindowMask |
                                      NSResizableWindowMask)
                             backing:NSBackingStoreBuffered
                               defer:NO];

  if (self) {
    self.effectIndex = effectIndex;
    self.filePath = filePath;
    [self addErrorLog]; // initialises self.textStorage and self.errorLog
    [self initFileWatchTimer]; // intialises self.fileModificationDate and self.timer

    NSRect bounds = [[NSScreen mainScreen] frame];

    [self makeKeyAndOrderFront:NSApp];
    [self setFrameTopLeftPoint: NSMakePoint(100,bounds.size.height - 100)];

    @autoreleasepool {
      [self compileAndLoadEffect];
    }
  }
  return self;
}

- (void) compileAndLoadEffect {
  [self makeFirstResponder: nil];
  if (self.openGLView) {
    [NSOpenGLContext clearCurrentContext];
    [self.openGLView clearGLContext];
    self.openGLView.openGLContext = nil;
    self.openGLView.pixelFormat = nil;
    [self.openGLView removeFromSuperview];
    [self.openGLView.timer invalidate];
    self.openGLView = nil;
  }

  if (self.controls) {
    for (NSControl *control in self.controls) {
      [control removeFromSuperview];
    }
    self.controls = nil;
  }
  char *uiSpecCString = msCompileAndLoadEffect(self.effectIndex,
                           [self.filePath cStringUsingEncoding: NSUTF8StringEncoding]);

  if (uiSpecCString[0] == '0' /* no errors */) {
    uiSpecCString++;
    NSString *uiSpecString = [[NSString alloc] initWithCString:uiSpecCString encoding: NSUTF8StringEncoding];
    NSError *error;
    NSArray *uiSpec = [MacShadyUIParser parseUISpec:uiSpecString error:&error];
    // FIXME: Handle error if it occurs

    [self setErrorLogText:@"Effect successfully compiled"];
    [self addControlsFromUISpec:uiSpec];
  } else { /* errors */
    uiSpecCString++;
    [self setErrorLogText:[[NSString alloc] initWithCString:uiSpecCString encoding:NSUTF8StringEncoding]];
  }
}

//
// Initialises self.fileModificationDate and self.timer
//
- (void) initFileWatchTimer {
    self.fileModificationDate = [self getFileModificationDate];

    self.timer = [NSTimer timerWithTimeInterval:1 // check every x seconds
                  target:self
                  selector:@selector(checkFileModified:)
                  userInfo:nil
                  repeats:YES];

    [[NSRunLoop currentRunLoop] addTimer:self.timer
                 forMode:NSDefaultRunLoopMode];

    [[NSRunLoop currentRunLoop] addTimer:self.timer
                 forMode:NSEventTrackingRunLoopMode];
}

- (void) addErrorLog {
  // See https://developer.apple.com/library/mac/documentation/TextFonts/Conceptual/
  //             CocoaTextArchitecture/TextSystemArchitecture/ArchitectureOverview.html#//
  //             apple_ref/doc/uid/TP40009459-CH7-SW10


  // NSTextStorage -> NSLayoutManager -> NSTextContainer -> NSTextView -> NSScrollView

  NSScrollView *errorLog = [[NSScrollView alloc] initWithFrame:[[self contentView] frame]];

  NSSize contentSize = [errorLog contentSize];

  errorLog.borderType = NSNoBorder;
  errorLog.hasVerticalScroller = YES;
  errorLog.hasHorizontalScroller = NO;
  errorLog.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
  errorLog.translatesAutoresizingMaskIntoConstraints = NO;

  NSTextStorage *textStorage = [[NSTextStorage alloc] initWithString:@""];

  NSLayoutManager *layoutManager = [[NSLayoutManager alloc] init];
  [textStorage addLayoutManager: layoutManager];

  NSTextContainer *textContainer =
    [[NSTextContainer alloc] initWithContainerSize:self.frame.size];

  [layoutManager addTextContainer:textContainer];

  NSTextView *textView = [[NSTextView alloc] initWithFrame:self.frame textContainer:textContainer];

  textView.editable = NO;
  textView.minSize = NSMakeSize(0.0, contentSize.height);
  textView.maxSize = NSMakeSize(FLT_MAX, FLT_MAX);
  textView.verticallyResizable = YES;
  textView.horizontallyResizable = NO;
  textView.autoresizingMask = NSViewWidthSizable;
  textContainer.containerSize = NSMakeSize(contentSize.width, FLT_MAX);
  textContainer.widthTracksTextView = YES;

  errorLog.documentView = textView;

  NSView *view = [self contentView];
  [view addSubview: errorLog];

  NSDictionary *views  = @{ @"errorLog": errorLog};
  NSArray *constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[errorLog(>=200)]-|"
                                               options: 0
                                               metrics:nil
                                               views:views];
  [view addConstraints: constraints];
  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[errorLog(==100)]-|"
                                               options: 0
                                               metrics:nil
                                               views:views];
  [view addConstraints: constraints];
  self.textStorage = textStorage;
  self.errorLog = errorLog;
}

- (NSDate *) getFileModificationDate {
  NSError *error = nil;
  NSDictionary *d = [[NSFileManager defaultManager] attributesOfItemAtPath: self.filePath
                                                    error:&error];
  if (!error) {
    return (NSDate *)[d fileModificationDate];
  } else {
    NSLog(@"%@", [error localizedDescription]);
  }
  return nil;
}


- (void) checkFileModified:(id )sender {
  NSDate *date = [self getFileModificationDate];
  if ([date compare: self.fileModificationDate] == NSOrderedDescending) {
    self.fileModificationDate = date;
    [self compileAndLoadEffect];
  }
}

- (void) setErrorLogText:(NSString *)message {
  NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:11];
  NSAttributedString *attrText =
    [[NSAttributedString alloc] initWithString:message
                                attributes:@{ NSFontAttributeName : menlo13 }];
  [self.textStorage setAttributedString:attrText];
  [self.errorLog.documentView scrollRangeToVisible:NSMakeRange([self.textStorage length], 0)];
  [self.errorLog flashScrollers];
}


//
// Precondition: addErrorLog must have already been called
//
- (void) addControlsFromUISpec:(NSArray *)uiSpec {
  NSRect frame = [self frame];


  NSView *view = [self contentView];

  NSControl *lastControl = nil;
  NSArray   *constraints;

  self.controls = [NSMutableArray array];

  NSEnumerator *enumerator = [uiSpec reverseObjectEnumerator];
  for (NSDictionary *uiElement in enumerator) {

    NSControl *control = [MacShadyUI controlFromUIElem: uiElement effectIndex:self.effectIndex];
    control.translatesAutoresizingMaskIntoConstraints = NO;

    [view addSubview: control];
    NSDictionary *views;
    if (lastControl) {
      views = @{ @"current": control, @"last": lastControl };
    } else {
      views = @{ @"current": control, @"errorLog": self.errorLog };
    }

    constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[current(>=200)]-|"
                                               options: 0
                                               metrics:nil
                                               views:views];
    [view addConstraints:constraints];

    if (lastControl) {
      constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[current(==20)]-[last(==20)]"
                                        options: 0
                                        metrics:nil
                                        views:views];
      [view addConstraints: constraints];

    } else {
      constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[current(==20)]-[errorLog(==100)]"
                                                            options: 0
                                                            metrics:nil
                                                            views:views];
      [view addConstraints: constraints];

    }
    lastControl = control;
    [self.controls addObject: control];
  }

  self.openGLView =
    [[MacShadyGLView alloc] initWithFrame: frame
                              effectIndex: self.effectIndex controls:self.controls];

  self.openGLView.translatesAutoresizingMaskIntoConstraints = NO;
  // Give the focus to this window
  [self makeFirstResponder:self.openGLView];
  [view addSubview:self.openGLView];

  NSDictionary *dict;
  if (lastControl) {
    dict = @{ @"glView": self.openGLView, @"errorLog": self.errorLog, @"last": lastControl };
  } else {
    dict = @{ @"glView": self.openGLView, @"errorLog": self.errorLog };
  }

  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[glView]-|"
                                                            options: 0
                                                            metrics:nil
                                                              views:dict];
  [view addConstraints:constraints];

  if (lastControl) {
    constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=50)]-[last(==20)]"
                                                   options: 0
                                                   metrics:nil
                                                   views:dict];
  } else {
   constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=50)]-[errorLog(==100)]"
                                                   options: 0
                                                   metrics:nil
                                                   views:dict];
  }
  [view addConstraints:constraints];
}



@end