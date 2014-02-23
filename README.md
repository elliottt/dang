
# Building dang

## Required build tools

 - alex-3.x
 - happy
 - cabal-install-1.18.x

## Building the source

```shell
$ ./setup.sh
$ cabal install
```

This should install all dependencies, and place the built compiler in the
build/bin directory.
