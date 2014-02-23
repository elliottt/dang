#!/bin/sh

set -e -u

if [ -d build ]; then
  echo "Build directory already exists.  Remove it if you really want to"
  echo "re-reun setup"
  exit 1
fi

git submodule update --init
cabal sandbox init --sandbox=build
cabal sandbox add-source deps/llvm-pretty
