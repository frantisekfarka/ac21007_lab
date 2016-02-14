Lab 5: Haskell
======================================

These lab exercises are yet again provided as a library. There are the following
files:

    * [Lab5.hs](./src/Lab5.hs) - the usual module with tasks for this lab,
        in folder 'src'
    * [Main.hs](./tests/Main.hs) - test-suit for tasks in Lab5

However, this time the cabal project file is missing. You need to initialise
this folder as a package yourself and create the project file. Follow the
instructions in this file and in case of any problems ask a tutor or refer to
cabal documentation (see earlier lab sheets)

A new project is initialised from within project folder using 'cabal' utility:

    ```sh
    cabal init
    ```

Cabal will prompt you for package specification -- e.g. package name, version,
license. Most of has a reasonable default value or can be empty, feel free to
provide any information you deem suitable. Although, we strongly recommend to
keep at least package name to default "lab05".

What you need to specify properly is what the package builds:

    ```
    What does the package build:
        1) Library
        2) Executable
    ```

In this case it is a library. Now open the newly generated file lab05.cabal (or
another name, depending on the name you provided previously) and check your new
project file. Note that a Cabal project file is indented using white space,
spaces only. Do not input any tab characters as this would result in a failure
while cabal reads the project file. Single-line comments in project file are 
introduce by two dashes (--)

You can see a general package information you specified and also a library
section, similar to:

```
    ...

    library
        exposed-modules:     Lab5
        build-depends:       base >=4.0 && <4.9
        hs-source-dirs:      src
        default-language:    Haskell2010

```

This section specifies that package 'lab05' provides a library consisting of
module Lab5, which is stored in source directory 'src'. The build-depends field
specifies which other libraries is this library using itself. In our case it is
only the standard 'base', the most basic definitions of Haskell, in any version
from 4.0 to 4.9 (excl). The default-language filed specifies that we are using 
Haskell 2010 language standard, you do not need to concern yourself with this
part. Please adjust your cabal project file accordingly.

Now you can work with this package using commands that you are used to from
previous sets of lab tasks with one exception -- the test suit is not visible
to Cabal so far. If you wish to use ``cabal test'' command you need to also
specify in the project file that there is a test suit. You can use the following
specification and paste it into your project file (be careful with whitespace):

```
  test-Suite lab05-test
    type:                 exitcode-stdio-1.0
    default-Language:     Haskell2010
    hs-source-dirs:       tests 
    ghc-options:          -Wall
    main-is:              Main.hs
    build-depends:        base >= 4.0 && <4.9
                          , QuickCheck
```
In this case, source dir, language, and dependencies fields work the same. Note
that we are using one new library -- QuickCheck. This is the library that
performs actual testing. We also specify type and main file with test. You do
not need to concert with these fields.

Once you create a project file with correct library and test-Suite sections, all
the functionality you are by now used to -- including haddock documentation -- is
available. You can proceed with tasks in [Lab5.hs](./src/Lab5.hs)











