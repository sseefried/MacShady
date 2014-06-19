//
//  ShadyUIGen.m
//  WindowUITest
//
//  Created by Sean Seefried on 11/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import "ShadyUIGen.h"

@implementation ShadyUIGen

+ (NSWindow *)uiFromSpec:(NSString *)uiSpecString effectIndex:(int)effectIndex error:(NSError **)error
{
  NSArray *uiSpec = [ShadyUIGen parseJSONSpec:uiSpecString error:error];
  if (!*error) {
    return [ShadyUIGen windowFromUISpec:uiSpec effectIndex: effectIndex];
  }
  return nil;
}

+ (NSArray *)parseJSONSpec:(NSString *)uiSpecString error:(NSError **)error
{
  NSString *errorDomain = @"ShadyUIGenError";
  NSData *data = [NSData dataWithBytes:[uiSpecString cStringUsingEncoding:NSASCIIStringEncoding]
                                length: uiSpecString.length];
  NSError *jsonError = nil;
  NSArray *uiSpec =
  [NSJSONSerialization JSONObjectWithData:data options:0 error:&jsonError];
  if (!jsonError) {
    if (![uiSpec isKindOfClass:[NSArray class]]) {
      *error = [NSError errorWithDomain:errorDomain code:2 userInfo: @{NSLocalizedDescriptionKey : @"Shady UI Specification must be a JSON Array"}];
      return nil;
    }

    for (id maybeJSONObject in uiSpec) {
      if (![maybeJSONObject isKindOfClass:[NSDictionary class]]) {
        *error = [NSError errorWithDomain:errorDomain code:3
                                 userInfo: @{NSLocalizedDescriptionKey : [NSString stringWithFormat: @"Shady UI Specification must be a JSON Array of JSON Objects. Instead found '%@'", maybeJSONObject]}];
        return nil;
      }
      NSDictionary *jsonDict = (NSDictionary *)maybeJSONObject;
      NSString *sort = [jsonDict valueForKey:@"sort"];
      if (!sort) {
        *error = [NSError errorWithDomain:errorDomain
                                     code:4
                                 userInfo: @{NSLocalizedDescriptionKey : [NSString stringWithFormat: @"JSON Object %@ does not contain \"sort\" key", maybeJSONObject]}];
        return nil;
      }
      if ([sort isEqualToString:@"float_slider"]) {
        // FIXME: Some more boring error work
      } else if ([sort isEqualToString:@"int_slider"]) {
        // FIXME: Some more boring error work
      } else if ([sort isEqualToString:@"time"]) {
        // FIXME: Some more boring error work
      } else {
        *error = [NSError errorWithDomain:errorDomain
                                     code:5
                                 userInfo: @{NSLocalizedDescriptionKey : [NSString stringWithFormat: @"Invalid sort for Shady UI element: \"%@\"", sort]}];
        return nil;
      }
    }

  } else {
    *error = [NSError errorWithDomain:errorDomain code:1 userInfo:[jsonError userInfo]];
    return nil;
  }
  return uiSpec;
}

+ (NSControl *)controlFromUIElem:(NSDictionary *)uiElement effectIndex:(int)effectIndex
{
  NSString *sort = [uiElement valueForKey:@"sort"];
  NSControl *control = nil;
  if ([sort isEqualToString:@"float_slider"]) {
    NSNumber *uniformIndex =  [uiElement valueForKey:@"glslUniformIndex"];
    NSNumber *minValue = [uiElement valueForKey:@"min"];
    NSNumber *value =    [uiElement valueForKey:@"value"];
    NSNumber *maxValue = [uiElement valueForKey:@"max"];
    NSNumber *ticks     = [uiElement valueForKey:@"ticks"];
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


+ (NSWindow *)windowFromUISpec:(NSArray *)uiSpec effectIndex:(int)effectIndex
{
  // Create a new dynamic window with a blue background
  CGFloat width = 800, height = 600;
  NSRect frame = NSMakeRect(0, 0, width, height);
  NSWindow *window  = [[NSWindow alloc] initWithContentRect:frame
                                                   styleMask:(NSTitledWindowMask |
                                                              NSClosableWindowMask |
                                                              NSResizableWindowMask)
                                                     backing:NSBackingStoreBuffered
                                                       defer:NO];
  NSRect bounds = [[NSScreen mainScreen] frame];

  //  [window setBackgroundColor:[NSColor blueColor]];
  [window makeKeyAndOrderFront:NSApp];
  [window setFrameTopLeftPoint: NSMakePoint(100,bounds.size.height - 100)];

  NSView *view = [window contentView];

  // Must initialise and add OpenGL view first so that GLSL program is compiled and linked
  // before uniform values are set by UI elements.

  MacShadyGLView *openGLView = [[MacShadyGLView alloc] initWithFrame: frame effectIndex: effectIndex];
  openGLView.translatesAutoresizingMaskIntoConstraints = NO;
  [view addSubview:openGLView];



  NSControl *lastControl = nil;
  NSArray   *constraints;

  NSEnumerator *enumerator = [uiSpec reverseObjectEnumerator];
  for (NSDictionary *uiElement in enumerator) {
    NSControl *control = [ShadyUIGen controlFromUIElem: uiElement effectIndex: effectIndex];
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
  }


  NSDictionary *dict = @{ @"glView" : openGLView, @"last": lastControl};

  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|-[glView]-|"
                                                          options: 0
                                                          metrics:nil
                                                            views:dict];
  [view addConstraints:constraints];

  constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[glView(>=20)]-[last(==20)]"
                                                 options: 0
                                                 metrics:nil
                                                 views:dict];
  [view addConstraints:constraints];
  return window;
}

@end
