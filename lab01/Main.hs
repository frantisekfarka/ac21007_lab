-- Module      :  Main
-- Author      :  Frantisek Farka <ffarka@dundee.ac.uk>
--
-- |
--
-----------------------------------------------------------------------------

module Main where


-- | Simple function to create a hello message.
hello :: String -> String
hello s = "Hello " ++ s ++ "!"

-- | Entry point of a program
main :: IO ()
main = putStrLn (hello "World")



-- Implement your own boolean functions:


-- | myAnd returns True only if both of the input are True.
--
-- myAnd :: Bool -> Bool -> Bool,

-- | myOr returns False only if both of the input are False.
--
-- myOr :: Bool -> Bool -> Bool

-- | myImply x y should return False only if x is True and y is False, for the
--   other cases it returnsTrue
--
-- myImply :: Bool -> Bool -> Bool

-- | myXor is called Exclusive disjunction(exclusive or), is a logical operation
--   that outputs True whenever both inputs differ (one is True, the other is False)
--
-- myXor :: Bool -> Bool -> Bool



-- | Define a function that converts boolean value to its coresponding text description,
--   e. g. 
--
--       boolToString True ==> "true"
--
-- boolToString :: Bool -> String



