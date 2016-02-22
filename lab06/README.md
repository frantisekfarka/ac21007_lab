Lab 5: Haskell
======================================

The task in this lab is to write a Haskell package that provides a program
(executable) with the following behaviour:

* The program takes a name of an input file as it's first command-line argument.
* The input file contains a list of numbers separated by a new line (see
  [file.in](./file.in) as an example of an input file).
* The program prompts user for a sorting algorithm to use. At least the following
  sorting algorithms are available:

  * Insertion Sort
  * Selection Sort
  * Bubble Sort
  * Quick Sort
* The program sorts the input file and writes a sorted list of numbers into an
  output file. The output is given as a second command-line argument. The output
  file format is the same as for the input file.


Note that almost all the parts of such a program have already been introduced
either in lecture slides or in previous labs. The only new part is cabal project
file specification of an executable. In fact, it is quite similar to a
specification of a library:

```cabal
    ...

    executable myProgram
        main-is:             Main.hs
        other-modules:       SortingAlgs
        build-depends:       base >=4.0 && <4.9
        hs-source-dirs:      src
        default-language:    Haskell2010

```

The above code specifies that a Cabal package provides an executable called
``myProgram``, the main entry point of this program is in a file Main.hs, and
all the source files are stored within the ``src`` directory. This executable
also uses an additional module (source file): ``SortingAlgs.hs``. You can
generate such a package description using the "cabal init" utility, which you
know from the last lab. The rest of the description of cabal package
specification in that labs also applies.


Bonus task
----------

As an additional task, extend your package in such a way, that it provides a
library of sorting algorithms beside the above binary. The above binary then
serves only as an interface to such a library and imports modules of this
library.



Haskell Modules
---------------

Until now we have been using Haskell modules only implicitly. This is a brief
summary of module system as it might be useful to separate source code in this
lab into multiple modules. For more information lease refer to Haskell, GHC and
Cabal documentation.

Haskell source code resided in /modules/. Each module is a collection of
top-level functions, data types, classes, etc. (we call these altogether
``bindings``) that are provided by one file. The file must have the same name as
the module in contains (except for the extension ``.hs``).  Names of modules can
also have hierarchical structure, e.g. ``Data.List``, and in that case this
module (``List``) must reside in a directory called ``Data`` in a file with name
``List.hs``.

The structure of a module is as follows:

```haskell
-- file ModuleName.hs
module ModuleName <export list> where

-- contents of the file
...

```

A name of a module begins with a capital letter. 
Export list a list of top-level 
bindings, exported by this module. The export list can be omitted in which case
all the top-level bindings are exported. If the export list is present, it has
the following structure:

```haskell
module ModuleName (

      function1
    , function2
    , DataType1 (Ctor1, Ctor2)
    , DataType2 (...)

    , TypeClass1 (method1, method2)
    , TypeClass2 (...)

) where

```

Bindings to be exported are separated by comma, in case of ADTs and Type Classes
we need to provide an explicit list of constructors and class methods
respectively that are exported. If we want to export all constructors of and ADT 
or all methods of a class we can specify this by an ellipsis (...).


Binding from another module must be imported using the following syntax:

```haskell
    import ModuleName <import list>
```

The import list is an inverse of the export list. It can be omitted and in that
case all the bindings exported by ``ModuleName`` are available, or certain
bindings can be specified using the same notation as in the export list.  You
can import any module that is local to a library or an executable you are
working with, or a module that is exported by a package that is listed in the
``build-depends`` filed in the cabal package description (This is why we always
specify at least base making available ``Prelude`` and some other standard
libraries). Note that ``Prelude`` module is imported implicitly and always
visible.  The fact that you can import a local module or a module in
``build-depends`` means that in order to use a module in a library in an
executable where the executable and the library share the same cabal package
description you need to specify this library in ``build-depends`` of the
executable.

The main module of an executable is specified in a module given in ``main-is``
filed of cabal package description. Convention is that this is a module called
``Main`` in file ``Main.hs``. This module must export a binding ``main`` with
the following type signature:

```haskell
    
main :: IO ()

```

This binding will be evaluated when the binary is called. 



