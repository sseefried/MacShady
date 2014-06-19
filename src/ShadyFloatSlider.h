//
//  ShadySlider.h
//  WindowUITest
//
//  Created by Sean Seefried on 10/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "ShadyControl.h"
#import "MacShadyHooks.h"

@interface ShadyFloatSlider : NSView <ShadyControl>

@property NSSlider    *slider;
@property NSTextField *titleLabel;
@property NSTextField *minLabel;
@property NSTextField *maxLabel;
@property NSTextField *valueLabel;
@property int         uniformIndex;
@property int         effectIndex;

- (id)initWithUniformIndex:(int)uniformIndex effectIndex:(int)effectIndex title:(NSString *)title
                  minValue:(double)minValue value:(double)value maxValue:(double)maxValue;
- (id)initWithUniformIndex:(int)uniformIndex effectIndex:(int)effectIndex title:(NSString *)title
                  minValue:(double)minValue value:(double)value maxValue:(double)maxValue
                     ticks:(NSInteger)ticks;

- (void)setGLSLUniform;

@end
