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


2. Use Haskell compiler(GHC) to compile the code to a standalone executable
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
