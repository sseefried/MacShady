# UI Language

## Introduction

The UI Language declaratively specifies a UI for a Shady effect. At the moment
it a very simple JSON based protocol. It is straightforward and non-rescursive.
(i.e. the grammar consists only of terminals). This may change in the future.

## UI Specification

A UI Specfication is simple a JSON array of UI Elements which are themselves
JSON Objects.

## UI Elements

There are 3 UI elements.

- Float Slider
- Integer Slider
- Time

1. All UI elements are encoded as JSON Objects. At a minimum each must have the
   "sort" key/value pair defined. Valid values of "sort" are specified below.
2. The "glslUniform" values must be
   a) valid GLSL uniforms
   b) not be the reserved uniform "time"
   c) be unique within a UI Specification (i.e. not repeated)
3. There may only be a maximum of one of one Time element.


### Float Slider

{   "sort":         "float_slider",
    "glslUniform":  <string>,
    "title":        <string>,
    "min":          <number>,
    "value":        <number>,
    "max":          <number>,
  [ "ticks":        <number> ] }

The "ticks" key/value pair is optional. The slider should be rendered with
a continuous slider bar.

### Integer Slider

{   "sort":         "int_slider",
    "glslUniform":  <string>,
    "title":        <string>,
    "min":          <number>,
    "value":        <number>,
    "max":          <number>,
  [ "ticks":        <number> ] }

JSON does not distinguish between integers and floating-point numbers. However,
each of the values in this JSON object is treated by the UI renderer as if it
is an integer by taking the floor of the value.

As before the "ticks" key/value pair is optional.

### Time

{   "sort":        "time" }

This UI element is special in that it is not (currently) rendered. As each
frame is rendered the "time" GLSL uniform is updated with current time.
Time starts at zero.