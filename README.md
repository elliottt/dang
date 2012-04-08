
# Building dang

## Required build tools

 - alex-2.3.5
   * This is necessary, as 3.x introduces a strange way of supporting UTF-8
 - happy

## Required libraries

 - GraphSCC
 - base
 - bytestring
 - cereal
 - containers
 - directory
 - filepath
 - monadLib
 - pretty
 - process
 - syb
 - template-haskell
 - text
 - https://github.com/elliottt/llvm-pretty

## Building the source

Dang builds using gnu make, and as such, you can build the binary by typing:

```shell
$ make
```

Once the build system finishes, you should be left with an executable called
`dang` in the current directory.

# Using dang

Dang doesn't really do anything except type-check at the moment.  Given that,
you'll want to run it with the -c option, to avoid having it try to link
anything.
