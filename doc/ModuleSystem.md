# Module System

## Syntax

  Modules are declared in a single file using the _module_ keyword, followed by
a qualified module name, then the _where_ keyword, and finally a block of
declarations.  The grammar looks something like this:

  _module_ ``name`` _where_ { ``decls`` }
