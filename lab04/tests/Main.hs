module Main where

import Control.Monad          (when)    
import Data.List as D         (sort, sortBy)
import System.Exit            (exitFailure)
import Test.QuickCheck (
      quickCheckWithResult
    , Testable
    , Args
    , stdArgs
--    , choose
--    , Gen
--    , elements
    , maxSuccess
    )

import Test.QuickCheck.Test   (isSuccess)


import Lab4 as L3 (
          selectionSortWith
        , insertionSortWith
        , bubbleSortWith
    )

-- ************************************************


prop_selsort :: [Int] -> Bool
prop_selsort xs = selectionSortWith id xs == sort xs

prop_inssort :: [(Char, (Int, Char))] -> Bool
prop_inssort xs = insertionSortWith g xs == sortBy f xs
    where
        g = fst . snd
        f a b = g a `compare` g  b

prop_bblsort :: [(String, Int)] -> Bool
prop_bblsort xs = bubbleSortWith g xs == sortBy f xs
    where
        g = (0 -) . snd
        f a b = g a `compare` g  b

-- | Helper to integrate QuicCheck with exitcode-stdio
quickCheckFailWith :: Testable prop => Args -> prop -> IO ()
quickCheckFailWith args p = do
        success <- fmap isSuccess $ quickCheckWithResult args p 
        when (not success) $ exitFailure -- exit if the result is a failure

-- | Helper, ditto
quickCheckFail :: Testable prop => prop -> IO ()
quickCheckFail p = quickCheckFailWith ourArgs p

ourArgs :: Args
ourArgs = stdArgs {
      maxSuccess = 500
    }

-- | Run all the properties
main :: IO ()
main = do
    putStrLn "\nselection sort:"
    quickCheckFail prop_selsort

    putStrLn "\ninsertion sort:"
    quickCheckFail prop_inssort

    putStrLn "\nbubble sort:"
    quickCheckFail prop_bblsort


