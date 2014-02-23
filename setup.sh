#!/bin/sh

git submodule update --init
cabal sandbox init --sandbox=build
cabal sandbox add-source deps/llvm-pretty
