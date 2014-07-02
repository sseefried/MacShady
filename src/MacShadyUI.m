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

- (id) initWithUISpec:(NSArray *)uiSpec effectIndex:(int)effectIndex {
  // Create a new dynamic meshCoords with a blue background

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
    self.filePath = @"dummy";
    [self addControlsFromUISpec:uiSpec];
  }

  return self;
}

- (void) addControlsFromUISpec:(NSArray *)uiSpec {
  NSRect frame = [self frame];

  NSRect bounds = [[NSScreen mainScreen] frame];

  [self makeKeyAndOrderFront:NSApp];
  [self setFrameTopLeftPoint: NSMakePoint(100,bounds.size.height - 100)];

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
      views = @{ @"current": control };
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
      constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:[current]-|"
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
     dict = @{ @"glView" : openGLView, @"last": lastControl};
  } else {
    dict = @{ @"glView" : openGLView };
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
   constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=20)]-|"
                                                   options: 0
                                                   metrics:nil
                                                   views:dict];
  }
  [view addConstraints:constraints];
}


@end