#!/bin/bash

rm -rf .cabal-sandbox
rm -f  cabal.sandbox.config

SB="cabal sandbox"

TOP_LEVEL_PACKAGES="vector-space-0.8.7 \
          wl-pprint-1.1 \
          type-unary-0.2.16\
          shady-graphics-0.6.1 \
          matrix-0.3.3.0 \
          OpenGL-2.9.1.0 \
          OpenGLRaw-1.4.0.0 \
          aeson-0.7.0.6 \
          plugins-1.5.4.0 \
          language-c-inline-0.6.0.0"




PACKAGES="Boolean-0.2.1 \
   GLURaw-1.4.0.1 \
   MemoTrie-0.6.2 \
   NumInstances-1.3 \
   OpenGL-2.9.1.0 \
   OpenGLRaw-1.4.0.0 \
   TypeCompose-0.9.10 \
   aeson-0.7.0.6 \
   applicative-numbers-0.1.3 \
   attoparsec-0.12.1.0 \
   cpphs-1.18.5 \
   data-treify-0.3.4 \
   dlist-0.7.0.1 \
   exception-mtl-0.3.0.4 \
   exception-transformers-0.3.0.3 \
   hashable-1.2.2.0 \
   haskell-src-exts-1.15.0.1 \
   haskell-src-meta-0.6.0.7 \
   language-c-inline-0.6.0.0 \
   language-c-quote-0.8.0 \
   mainland-pretty-0.2.7 \
   matrix-0.3.3.0 \
   mtl-2.1.3.1 \
   nats-0.2 \
   newtype-0.2 \
   polyparse-1.9 \
   primitive-0.5.3.0 \
   scientific-0.3.2.1 \
   semigroups-0.15.1 \
   shady-gen-0.6.3 \
   shady-graphics-0.6.1 \
   srcloc-0.4.0 \
   stm-2.4.3 \
   syb-0.4.2 \
   symbol-0.2.1 \
   text-1.1.1.3 \
   th-lift-0.6.1 \
   th-orphans-0.8.1 \
   transformers-0.3.0.0 \
   ty-0.1.6 \
   type-unary-0.2.16 \
   unordered-containers-0.2.4.0 \
   vector-0.10.11.0 \
   vector-space-0.8.7 \
   void-0.6.1 \
   plugins-1.5.4.0 \
   wl-pprint-1.1"

$SB init

SDIR="extra-src-repos"
mkdir -p $SDIR

for i in shady-gen shady-graphics; do
  rm -rf $SDIR/$i
  (cd $SDIR; git clone https://github.com/sseefried/$i)
  $SB add-source $SDIR/$i
done

#
# For release generate the PACKAGES string from TOP_LEVEL_PACKAGES
#
# Change to $TOP_LEVEL_PACKAGES below. Then do a "ghc-pkg list" and copy out the exact packages
# required into this file.
#

cabal install -j $PACKAGES