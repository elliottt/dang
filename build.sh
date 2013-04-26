#!/bin/sh

export PATH=$PWD/cabal-dev/bin:$PATH

if test ! -f cabal-dev/bin/happy; then
	cabal-dev install happy
fi

if test ! -f cabal-dev/bin/alex; then
	cabal-dev install alex
fi

cabal-dev install ./deps/llvm-pretty .
