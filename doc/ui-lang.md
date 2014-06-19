# UI Language

## Introduction

The UI Language declaratively specifies a UI for a Shady effect. It was
designed so that a foreign language (i.e. not Haskell) GUI library can interface
with a Haskell program. We'll call the foreign language part the "client"
and the Haskell part the "server" even though they are not communicating
over a network or IPC but via Haskell's FFI.

At the moment it is a very simple JSON based protocol. It is straightforward and
non-rescursive. (i.e. the grammar consists only of terminals). This may change
in the future.

The design philosophy behind the protocol is that it only specifies the
minimum amount of information required for the GUI library to render
the user interface elements.

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

2. Each "glslUniformIndex" value must be a unique integer (within the
   UI Specification) that identifies a GLSL uniform location in the
   compiled GLSL program. The server is expected to have a mapping
   from the uniform index to a GLSL uniform location (see the GLSL spec)
   so that it can set the value.

3. There may only be a maximum of one of one Time element.

### Float Slider

{   "sort":         "float_slider",
    "glslUniformIndex":  <number>,
    "title":             <string>,
    "min":               <number>,
    "value":             <number>,
    "max":               <number>,
  [ "ticks":             <number> ] }

The "ticks" key/value pair is optional. The slider should be rendered with
a continuous slider bar.

### Integer Slider

{   "sort":             "int_slider",
    "glslUniformIndex": <number>,
    "title":            <string>,
    "min":              <number>,
    "value":            <number>,
    "max":              <number>,
  [ "ticks":            <number> ] }

JSON does not distinguish between integers and floating-point numbers. However,
each of the values in this JSON object is treated by the UI renderer as if it
is an integer by taking the floor of the value.

As before the "ticks" key/value pair is optional.

### Time

{   "sort":        "time" }

This UI element is special in that it is not (currently) rendered. As each
frame is rendered the "time" GLSL uniform is updated with current time.
Time starts at zero.