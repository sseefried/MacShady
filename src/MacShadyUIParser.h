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



@interface MacShadyUIParser : NSObject

+ (NSArray *)parseUISpec:(NSString *)uiSpecString error:(NSError **)error;

@end
