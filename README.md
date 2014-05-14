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