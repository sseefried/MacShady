# MacShady
### the shady Scotsman

## How I generated the nib file

The code depends on the .nib file in MacShady.app/Contents/Resources/Base.lproj/MainMenu.nib being correct.

The nib file was generated in Interface Builder in a skeleton project in Xcode
5. It consists of a Window containing an NSView containing an NSOpenGLView

- the class of the NSOpenGLView is set to MacShadyGLView
- a referencing outlet from the NSOpenGLView goes to "delegate" of Window
- Constraints are add between the NSView and the NSOpenGLView
  - Leading space to Superview
  - Top Space to Superview
  - Bottom space to Superview
  - Trailing space to Superview

## OpenGL Core Profile 3.2

In order to have the shaders display I had


## Problems overcome

Q1. I had a torus displaying but it looked funny. First there was a circle
  kind of showing at one end and there was a strange stripey nature to it.
  I eventually worked out that it must be that the z test wasn't working
  properly.

A1. It turned out that I had not enabled a depth buffer in Cocoa's
    NSOpenGLView. I could have done this in the interface builder but
    instead I opted to override it in the awakeFromNib method.

    I also had to set the right depthFunc using the Haskell OpenGL library.

      depthFunc $= Just Less