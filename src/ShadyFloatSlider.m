//
//  ShadySlider.m
//  WindowUITest
//
//  Created by Sean Seefried on 10/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import "ShadyFloatSlider.h"

@implementation ShadyFloatSlider

- (id)initWithUniformIndex:(int)uniformIndex effectIndex:(int)effectIndex title:(NSString *)title
                  minValue:(double)minValue value:(double)value maxValue:(double)maxValue
{
  return [self initWithUniformIndex:uniformIndex effectIndex: effectIndex title:title
                           minValue:minValue value:value maxValue:maxValue ticks:0];
}


/*
 * 'ticks' is an optional parameter. If it is zero then the slider is continuous.
 */
- (id)initWithUniformIndex:(int)uniformIndex effectIndex:(int)effectIndex title:(NSString *)title
                  minValue:(double)minValue value:(double)value maxValue:(double)maxValue
                     ticks:(NSInteger)ticks
{
    self = [super initWithFrame:NSMakeRect(0,0,100,100)];
    if (self) {
      self.uniformIndex = uniformIndex;
      self.titleLabel = [[NSTextField alloc] init];
      self.minLabel   = [[NSTextField alloc] init];
      self.maxLabel   = [[NSTextField alloc] init];
      self.valueLabel = [[NSTextField alloc] init];

      self.titleLabel.stringValue = title;
      self.minLabel.stringValue   = [NSString stringWithFormat:@"%.2f", minValue];
      self.maxLabel.stringValue   = [NSString stringWithFormat:@"%.2f", maxValue];
      self.valueLabel.stringValue = [NSString stringWithFormat:@"%.2f", value];
      self.effectIndex            = effectIndex;


      for (NSTextField *tf in @[self.titleLabel, self.minLabel, self.valueLabel, self.maxLabel]) {
        [self addSubview: tf];
        tf.translatesAutoresizingMaskIntoConstraints = NO;
        tf.editable = NO;
        tf.drawsBackground = NO;
        tf.bezeled = NO;
      }

      self.slider = [[NSSlider alloc] init];
      [self addSubview:self.slider];
      [self.slider setMinValue:minValue];
      [self.slider setMaxValue:maxValue];
      [self.slider setTarget:self];
      [self.slider setAction:@selector(setGLSLUniform)];
      [self.slider setTranslatesAutoresizingMaskIntoConstraints:NO];
      self.slider.doubleValue = value;

      if (ticks > 0) {
        self.slider.numberOfTickMarks = ticks+1;
        self.slider.allowsTickMarkValuesOnly = YES;
      }

      NSDictionary *views = @{@"slider": self.slider, @"title": self.titleLabel, @"min": self.minLabel,
                                        @"max": self.maxLabel, @"value": self.valueLabel };

      NSArray *constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"|[title]-[min]-[slider]-[max]-[value]|"
                                                                     options: 0
                                                                     metrics:nil
                                                                     views:views];
      [self addConstraints:constraints];
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect
{
    [super drawRect:dirtyRect];
}

- (void)setGLSLUniform
{
  msSetFloatUniform(self.effectIndex, self.uniformIndex, self.slider.floatValue);
  self.valueLabel.stringValue = [NSString stringWithFormat:@"%.2f", self.slider.doubleValue];
}

@end
