Lab 1: Haskell Programming, 9.00-10.00
======================================

We advice you to use a Linux machine in the Lab 2 to do this exercise if possible.

1. Obtain a copy of this repository according to the following instructions.
    1. In a terminal window, clone the git repository to your local machine and
        open *lab01* directory:

        ```
        git clone https://github.com/frantisekfarka/ac21007_lab.git
        cd ac21007_lab/lab01
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
   3. You can then run the executable from the terminal (./hello on Unix
      systems, hello.exe on
      Windows):

        ```
            $ ./hello
            Hello, World!
        ```

3. Use Haskell interpreter (GHCi) to test the file *Main.hs*. Haskell
   interpreter reads expressions from command line and evaluates it. The
   beaviour of interpterter is controlled by command prefixed with ":"

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

    3. Run the function `hello` from GHCi with some atgument, e.g.

        ```
            *Main> hello "Franta"
            "Hello Franta!"
        ```

    4. Edit the file *Main.hs* such that functin `hello` outputs the text
       "Goodbye" while keeping the original GHCi shell open. Then go to the
       shell and reload the file from within the shell using the command
       `:reload`:

       ```
            *Main> :reload 
            Ok, modules loaded: Main.
       ```

    5. Test the new behavior of function `hello`:

        ```
            *Main> hello "Franta"
            "Goodbye Franta!"
        ```

    6. Exit the GHCi shell by a command `:quit`

    7. Recompile your *Main.hs* and test the new binary!




