//
//  Created by Sean Seefried on 11/06/2014.
//  Copyright (c) 2014 SeefriedSoftware. All rights reserved.
//

#import "MacShadyUIParser.h"

@implementation MacShadyUIParser

+ (NSArray *)parseUISpec:(NSString *)uiSpecString error:(NSError **)error
{
  NSArray *uiSpec = [MacShadyUIParser parseJSONSpec:uiSpecString error:error];
  if (!*error) {
    return uiSpec;
  }
  return nil;
}

+ (NSArray *)parseJSONSpec:(NSString *)uiSpecString error:(NSError **)error
{
  NSString *errorDomain = @"MacShadyUIParserError";
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


@end
