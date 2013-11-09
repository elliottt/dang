#!/bin/sh

cabal sandbox init

mkdir -p build-deps
git clone https://github.com/elliottt/llvm-pretty ./build-deps/llvm-pretty
cabal sandbox add-source build-deps/llvm-pretty

