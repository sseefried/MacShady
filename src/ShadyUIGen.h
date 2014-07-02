//
//  ShadyUIGen.h
//  WindowUITest
//
//  Created by Sean Seefried on 11/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>

#import "HsFFI.h"
#import "MacShadyUI.h"



@interface ShadyUIGen : NSObject

+ (MacShadyUI *)uiFromSpec:(NSString *)uiSpecString
                effectIndex:(int)effectIndex error:(NSError **)error;

@end
