-- Module      :  Main
-- Author      :  Frantisek Farka <ffarka@dundee.ac.uk>
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

-- | Simple function to create a hello message.
hello :: String -> String
hello s = "Hello " ++ s ++ "!"

-- | Entry point of a program
main :: IO ()
main = putStrLn (hello "World")



