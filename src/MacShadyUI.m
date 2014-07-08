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
    self.errorLog = [self addErrorLog];

    NSRect bounds = [[NSScreen mainScreen] frame];

    [self makeKeyAndOrderFront:NSApp];
    [self setFrameTopLeftPoint: NSMakePoint(100,bounds.size.height - 100)];


    char *uiSpecCString =
      msCompileAndLoadEffect(effectIndex, [filePath cStringUsingEncoding: NSUTF8StringEncoding]);

    if (uiSpecCString[0] == '0' /* no errors */) {
      uiSpecCString++;
      NSString *uiSpecString = [[NSString alloc] initWithCString:uiSpecCString encoding: NSUTF8StringEncoding];



      NSError *error;
      NSArray *uiSpec = [MacShadyUIParser parseUISpec:uiSpecString error:&error];
      // FIXME: Handle error if it occurs

      [self setErrorLogText:@"Success!"];
      [self addControlsFromUISpec:uiSpec];
    } else {
      uiSpecCString++;
      [self setErrorLogText:[[NSString alloc] initWithCString:uiSpecCString encoding:NSUTF8StringEncoding]];
    }
  }

  return self;
}

- (NSTextView *) addErrorLog {
  // See https://developer.apple.com/library/mac/documentation/TextFonts/Conceptual/
  //             CocoaTextArchitecture/TextSystemArchitecture/ArchitectureOverview.html#//
  //             apple_ref/doc/uid/TP40009459-CH7-SW10


  // NSTextStorage -> NSLayoutManager -> NSTextContainer -> NSTextView

  self.textStorage = [[NSTextStorage alloc] initWithString:@""];

  NSLayoutManager *layoutManager = [[NSLayoutManager alloc] init];
  [self.textStorage addLayoutManager: layoutManager];

  NSTextContainer *textContainer =
    [[NSTextContainer alloc] initWithContainerSize:self.frame.size];

  [layoutManager addTextContainer:textContainer];

  NSTextView *errorLog = [[NSTextView alloc] initWithFrame:self.frame textContainer:textContainer];
  errorLog.translatesAutoresizingMaskIntoConstraints = NO;
  errorLog.editable = NO;

  NSView *view = [self contentView];
  [view addSubview: errorLog];

  NSDictionary *views  = @{ @"errorLog": errorLog};
  NSArray *constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[errorLog(>=200)]-|"
                                               options: 0
                                               metrics:nil
                                               views:views];
  [view addConstraints: constraints];
  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[errorLog(==80)]-|"
                                               options: 0
                                               metrics:nil
                                               views:views];
  [view addConstraints: constraints];
  return errorLog;
}

- (void) setErrorLogText:(NSString *)message {
  NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:13];
  NSAttributedString *attrText =
    [[NSAttributedString alloc] initWithString:message
                                attributes:@{ NSFontAttributeName : menlo13 }];
  NSLog(@"%@", attrText);
  [self.errorLog.textStorage setAttributedString:attrText];
//  [self.errorLog scrollRangeToVisible:NSMakeRange([self.errorLog.textStorage length], 0)];

}


//
// Precondition: addErrorLog must have already been called
//
- (void) addControlsFromUISpec:(NSArray *)uiSpec {
  NSRect frame = [self frame];


  NSView *view = [self contentView];

  NSControl *lastControl = nil;
  NSArray   *constraints;

  NSMutableArray *controls = [NSMutableArray array];

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
      constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[current(==20)]-[errorLog]"
                                                            options: 0
                                                            metrics:nil
                                                            views:views];
      [view addConstraints: constraints];

    }
    lastControl = control;
    [controls addObject: control];
  }

  MacShadyGLView *openGLView =
    [[MacShadyGLView alloc] initWithFrame: frame
                              effectIndex: self.effectIndex controls:controls];
  openGLView.translatesAutoresizingMaskIntoConstraints = NO;
  // Give the focus to this window
  [self makeFirstResponder:openGLView];
  [view addSubview:openGLView];


  NSDictionary *dict;
  if (lastControl) {
    dict = @{ @"glView": openGLView, @"errorLog": self.errorLog, @"last": lastControl };
  } else {
    dict = @{ @"glView": openGLView, @"errorLog": self.errorLog };
  }

  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[glView]-|"
                                                            options: 0
                                                            metrics:nil
                                                              views:dict];
  [view addConstraints:constraints];

  if (lastControl) {
    constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=20)]-[last(==20)]"
                                                   options: 0
                                                   metrics:nil
                                                   views:dict];
  } else {
   constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=20)]-[errorLog]"
                                                   options: 0
                                                   metrics:nil
                                                   views:dict];
  }
  [view addConstraints:constraints];
}


@end