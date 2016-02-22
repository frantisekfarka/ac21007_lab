Lab 5: Haskell
======================================

The task in this lab is to write a Haskell package that provides a program
(executable) with the following behaviour:

* The program takes a name of an input file as it's first command-line argument.
* The input file contains a list of numbers separated by a new line (see
  [file.in](./file.in) as an example of an input file).
* The program promts user for a sorting algorithm to use. At least the following
  sorting algoritms are available:

  * Insertion Sort
  * Selection Sort
  * Bubble Sort
  * Quick Sort
* The program sorts the input file and writes a sorted list of numbers into an
  output file. The output is given as a second command-line argument. The output
  file format is the same as for the input file.


Note that almost all the parts of such a program have already been introduced
either in lecture slides or in previos labs. The only new part is cabal project
file specification of an executable. In fact, it is quite similar to a
specification of a library:
```
    ...

    executable myProgram
        main-is:             Main.hs
        other-modules:       SortingAlgs
        build-depends:       base >=4.0 && <4.9
        hs-source-dirs:      src
        default-language:    Haskell2010

```

The above code specifies that a Cabal package proveds an executable called
``myProgram'', the main entry point of this program is in a file Main.hs, and
all the source files are stored within the ``src'' directory. This executable
also uses an additional module (source file): ``SortingAlgs.hs''. YOu can
generate such a package description using the "cabal init" utility, which you
know from the last lab. The rest of the description of cabal package
specification in that labs also applies.


Bonus task
----------

As an additional taks, you may extend your package in such a way, that it
provides a library of sorting algorithms beside the above binary. The abouve
binary then provides only an interface to such a library and imports
modules of this library.





