//
//  ShadySlider.h
//  WindowUITest
//
//  Created by Sean Seefried on 10/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface ShadyFloatSlider : NSView

@property NSSlider    *slider;
@property NSTextField *titleLabel;
@property NSTextField *minLabel;
@property NSTextField *maxLabel;
@property NSTextField *valueLabel;
@property NSString    *uniform;

- (id)initWithUniform:(NSString*)uniform title:(NSString *)title minValue:(double)minValue value:(double)value maxValue:(double)maxValue;
- (id)initWithUniform:(NSString*)uniform title:(NSString *)title minValue:(double)minValue value:(double)value maxValue:(double)maxValue ticks:(NSInteger)ticks;

@end
