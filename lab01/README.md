Lab 1: Haskell Programming, 9.00-10.00
======================================

We advice you to use a Linux machine in the Lab 2 to do this exercise if possible.

1. Obtain a copy of this repository according to the following instructions.
    1. In a terminal window, clone the git repository to your local machine and
        open *lab01* directory:

        ```
        $ git clone https://github.com/frantisekfarka/ac21007_lab.git
        $ cd ac21007_lab/lab01
        ```

    2. If you are not familiar with version control system *git* and want to
        know more, see the Git Book (https://git-scm.com/book)


2. Use Haskell compiler (GHC) to compile the code to a standalone executable
   according to the following instructions.

   1. Open file *Main.hs* in a text editor and inspect it.
   2. Compile it in the terminal with:

        ```
        $ ghc -o main Main.hs
        ```
   3. You can then run the executable from the terminal (`./main` on Unix
      systems, `main.exe` on Windows):

        ```
        $ ./main
        Hello, World!
        ```

3. Use Haskell interpreter (GHCi) to test the file *Main.hs*. Haskell
   interpreter reads expressions from command line and evaluates it. The
   behaviour of interpreter is controlled by command prefixed with ":"

   1. Load the file *Main.hs* to GHCi:

        ```
        $ ghci Main.hs
        GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
        [1 of 1] Compiling Main             ( Main.hs, interpreted )
        Ok, modules loaded: Main.
        *Main> 
        ```

        or alternatively run plain GHCi shell and use a command `:load <module>` to
        load it:

        ```
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

        or alternatively get more information using a command `:into <function>`

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

    2. Initialise new development environment (sandbox) for a package and
       configure the package (in this order):

       ```
       $ cabal sandbox init
       $ cabal configure
       ```
    3. Now the environment is prepared and you can instruct cabal to build and
       run the executable for you:

       ```
       $ cabal run
       ```

       you can also ask cabal to run GHCi while loading the main file:

       ```
       $ cabal repl
       ```
    
       (note: repl stands for "read--eval--print loop", which is what GHCi does)


    4. Bonus: You can try to generate documentation for this package:

       ```
       $ cabal haddock --executables
       ```

       The generated documentation is stored in *dist/doc/html/lab01/lab01/index.html*


5. Implement your own boolean functions `myAnd`, `myOr`, `myImply`, and `myXor`.
   Behaviour of these function is described in comments in the file *Main.hs*
   For each function uncommon the type signature and provide an implementation.

6. Define a function that converts boolean value to its textual description. Yet
   again the specification and type signature is in comments in *Main.hs*


