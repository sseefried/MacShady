HC      = ghc
VER     = 7.6.3
CFLAGS  = -fobjc-arc -I$(shell $(HC) --print-libdir)/include
HCFLAGS = -package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d \
          -no-user-package-db \
          -package OpenGLRaw

LDFLAGS = -package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d \
          -no-user-package-db \
          -package template-haskell -package language-c-quote \
          -package language-c-inline -package hint \
          -package OpenGLRaw \
          -framework Cocoa -framework OpenGL -optl-ObjC -threaded

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o NSLog.o NSLog_objc.o

default: MacShady.app/Contents/MacOS/MacShady

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)


AppDelegate.o:
App.o: NSLog.o
Main.o: App.o AppDelegate.o

App_objc.m: App.o
AppDelegate_objc.m: AppDelegate.o

MacShady: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

MacShady.app/Contents/MacOS/MacShady: MacShady
	cp $< $@

.PHONY: clean

clean:
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] *_stub.h NSLog_objc.[hm] \
	   MacShady \
	   MacShady.app/Contents/MacOS/MacShady
