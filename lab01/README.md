Lab 1: Haskell 
======================================

Please either use a Linux machine in the Lab 2 or install
Haskell Platform on your own machine. Installation packages are available for
Linux, Mac, and Windows:

```
https://www.haskell.org/platform/
```

Haskell distribution on lab machines is slightly outdated (GHC 7.6) and the
version of Cabal build system does not support some newer features, which you get when
you install the newest platform on your own. Please pay attention to the
following instructions as the two options (older and newer Cabal) are slightly 
different:


1. Obtain a copy of this repository according to the following instructions.
    1. In a terminal window, clone the git repository to your local machine and
        open *lab01* directory:

        ```sh
        $ git clone https://github.com/frantisekfarka/ac21007_lab.git
        $ cd ac21007_lab/lab01
        ```

    2. If you are not familiar with version control system *git* and want to
        know more, see the Git Book (https://git-scm.com/book)


2. Use Haskell compiler (GHC) to compile the code to a standalone executable
   according to the following instructions.

   1. Open file *Main.hs* in a text editor and inspect it.
   2. Compile it in the terminal with:

        ```sh
        $ ghc -o helloWorld Main.hs
        ```
   3. You can then run the executable from the terminal (`./helloWorld` on Unix
      systems, `helloWorld.exe` on Windows):

        ```sh
        $ ./helloWorld
        Hello, World!
        ```

3. Use Haskell interpreter (GHCi) to test the file *Main.hs*. Haskell
   interpreter reads expressions from command line and evaluates it. The
   behaviour of interpreter is controlled by command prefixed with ":"

   1. Load the file *Main.hs* to GHCi:

        ```sh
        $ ghci Main.hs
        GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
        [1 of 1] Compiling Main             ( Main.hs, interpreted )
        Ok, modules loaded: Main.
        *Main> 
        ```

        or alternatively run plain GHCi shell and use a command `:load <module>` to
        load it:

        ```sh
        $ ghci
        GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
        Prelude> :load Main.hs
        [1 of 1] Compiling Main             ( Main.hs, interpreted )
        Ok, modules loaded: Main.
        ```

        Note that tab-completion should work within GHCi shell.

    2. When in GHCi, verify the type of function `hello` using a command:

        ```
        *Main> :type hello
        hello :: String -> String
        ```

        or alternatively get more information using a command `:info <function>`

    3. Run the function `hello` from GHCi with some argument, e.g.

        ```
        *Main> hello "Franta"
        "Hello Franta!"
        ```

    4. Edit the file *Main.hs* such that function `hello` outputs the text
       "Goodbye" while keeping the original GHCi shell open. Then go to the
       shell and reload the file from within the shell using the command
       `:reload`:

       ```
       *Main> :reload 
       Ok, modules loaded: Main.
       ```

    5. Test the new behaviour of function `hello`:

        ```
        *Main> hello "Franta"
        "Goodbye Franta!"
        ```

    6. Exit the GHCi shell by a command `:quit`

    7. Recompile your *Main.hs* and test the new binary!



4. Now try to do the same using Cabal.

    1. The source files for this lab are provided as a Haskell package.
        There is a package description in the file *lab01.cabal* However, you do
        not need to concern yourself with contents of this file


    2. This step differs for the new and old version of cabal:

        1. For the Cabal with ghc 7.8 and on (your machine):
    
            Initialise new development environment (sandbox) for a package and
            configure the package (in this order):

            ```sh
            $ cabal sandbox init
            $ cabal configure
            ```

            Now the environment is prepared and you can instruct cabal to build and
            run the executable for you:

            ```sh
            $ cabal build
            $ cabal run
            ```

            you can also ask cabal to run GHCi while loading the main file:

            ```sh
            $ cabal repl
            ```
    
            (note: repl stands for "read--eval--print loop", which is what GHCi does)

        2. For the Cabal with ghc 7.6 and on (lab machine):
    
            Configure the package: 

            ```sh
            $ cabal configure
            ```

            Now the environment is prepared and you can instruct cabal to build 
            the executable for you:

            ```sh
            $ cabal build
            ```

            However, you need to run the built executable manually:

            ```sh
            $ ./dist/build/lab01/lab01
            ```
    

    4. Bonus: You can try to generate documentation for this package:

       ```sh
       $ cabal haddock --executables
       ```

       The generated documentation is stored in *dist/doc/html/lab01/lab01/index.html*


5. Implement your own boolean functions `myAnd`, `myOr`, `myImply`, and `myXor`.
   Behaviour of these functions is described in comments in the file *Main.hs*.
   For each function uncomment the type signature and provide an implementation.

6. Define a function that converts boolean value to its textual description. Yet
   again the specification and type signature is in comments in *Main.hs*
