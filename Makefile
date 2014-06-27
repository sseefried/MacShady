HC      = ghc
VER     = 7.6.3
SRC     = src
CC      = /usr/bin/cc
CFLAGS  = -fobjc-arc -I$(shell $(HC) --print-libdir)/include -I$(SRC)
HCFLAGS = -package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d \
          -no-user-package-db \
          -i$(SRC)

LDFLAGS = -package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d \
          -no-user-package-db \
          -package vector-space \
          -package wl-pprint \
          -package type-unary \
          -package shady-graphics \
          -package matrix \
          -package OpenGL \
          -package OpenGLRaw \
          -package aeson \
          -framework Cocoa -framework OpenGL -optl-ObjC -threaded

OBJS = $(SRC)/Main.o \
       $(SRC)/App.o \
       $(SRC)/Hooks.o \
       $(SRC)/ShaderUtil.o \
       $(SRC)/Shady/CompileEffect.o \
       $(SRC)/Shady/TestEffect.o \
       $(SRC)/MSState.o \
       $(SRC)/MatrixUtil.o \
       $(SRC)/App_objc.o \
       $(SRC)/NSLog_objc.o \
       $(SRC)/AppDelegate.o \
       $(SRC)/AppDelegate_objc.o \
       $(SRC)/ShadyFloatSlider.o \
       $(SRC)/ShadyUIGen.o \
       $(SRC)/MacShadyGLView.o \
       $(SRC)/NSLog.o \
       $(SRC)/CocoaKey.o

HI_FILES=$(patsubst %.o,%.hi,$(OBJS))

default: MacShady.app/Contents/MacOS/MacShady

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

$(SRC)/MacShadyGLView.o: $(SRC)/MacShadyGLView.h $(SRC)/MacShadyHooks.h $(SRC)/ShadyControl.h
$(SRC)/ShadyFloatSlider.o: $(SRC)/ShadyFloatSlider.h $(SRC)/MacShadyHooks.h $(SRC)/ShadyControl.h
$(SRC)/ShadyUIGen.o: $(SRC)/ShadyUIGen.h $(SRC)/MacShadyGLView.o

$(SRC)/AppDelegate.o: $(SRC)/MSState.o $(SRC)/ShadyUIGen.o $(SRC)/Shady/TestEffect.o

$(SRC)/Main.o: $(SRC)/Shady/CompileEffect.o

$(SRC)/Hooks.o:       $(SRC)/ShaderUtil.o $(SRC)/MatrixUtil.o $(SRC)/MSState.o \
	                    $(SRC)/Shady/CompileEffect.o
$(SRC)/App.o:         $(SRC)/NSLog.o
$(SRC)/Main.o:        $(SRC)/App.o $(SRC)/AppDelegate.o

$(SRC)/NSLog_objc.m:       $(SRC)/NSLog.o
$(SRC)/App_objc.m:         $(SRC)/App.o
$(SRC)/AppDelegate_objc.m: $(SRC)/AppDelegate.o

$(SRC)/MSState.o: $(SRC)/Shady/CompileEffect.o $(SRC)/CocoaKey.o

MacShady: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

MacShady.app/Contents/MacOS/MacShady: MacShady
	cp $< $@

.PHONY: clean

clean:
	rm -f $(OBJS) $(HI_FILES) $(SRC)/App_objc.[hm]\
        $(SRC)/AppDelegate_objc.[hm] $(SRC)/*_stub.h $(SRC)/NSLog_objc.[hm] \
        MacShady \
	      MacShady.app/Contents/MacOS/MacShady
