AC2007 Module @ University of Dundee - Haskell Part
==============================================================

Lecture Slides
---------------

* [Lecture 1: An Introduction to Haskell](slides/lec1.pdf)
* [Lecture 2: List functions, function polymorphism, non-strict semantics](slides/lec2.pdf)
* [Lecture 3: Non-strict semantics, tuples](slides/lec3.pdf)
* [Lecture 4: Higher order functions, map, folds ](slides/lec4.pdf)
* [Lecture 5: Selection Sort, Insertion Sort, and Bubble Sort](slides/lec5.pdf)
* [Lecture 6: Tail Recursion, Algebraic Data Types, Type Classes](slides/lec6.pdf)
* [Lecture 7: Quick Sort, Monadic IO](slides/lec7.pdf)


Lab Assignments
---------------

*   [Lab 1](lab01/README.md)
    - environment set-up
    - ghc + ghci - Hello World!
    - logic functions 

* [Lab 2](lab02/README.md)
    - basic cabal (sandbox, configure, build, test, haddock)
    - list functions:
        - deletion, search, append, revprefix, forAll, exists, insertAt, sortList

* [Lab 3](lab03/README.md)
    - some higher order functions and folds
        - *filter*, *map*: partition, multiplyMatrix
        - *folds*: myConcat, myMinimum, subsequences, transposeMatrix 

* [Lab 4](lab04/README.md)
    - sort implementations with an accessor function
        - Selection Sort
        - Insertion Sort
        - Bubble Sort

* [Lab 5](lab05/README.md)
    - cabal project file
    - tail recursion
        - alg vs. algT
    - folds
        - sumSquare
        - split
        - merge
    - reconstructing data type from functions and instances

* [Lab 6](lab06/README.md)
    - putting all the pieces together
    - a binary that
        - takes input and output file from command line
        - prompts for a sorting algorithm to use
        - sort the input file and writes it to the output file
    - no cabal package provided
