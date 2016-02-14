module Main where

import Control.Monad          (when)    
--import Data.List as D         (sort, sortBy)
import System.Exit            (exitFailure)
import Test.QuickCheck (
      quickCheckWithResult
    , Testable
    , Args
    , stdArgs
--    , choose
--    , Gen
--    , elements
    , Positive(..)
    , maxSuccess
    )

import Test.QuickCheck.Test   (isSuccess)


import Lab5 as L5 (
          alg
        , algT
        , sumSquare
        , split
        , merge
    )

-- ************************************************

propAlg :: Bool
propAlg = fmap L5.alg [1..20] == fmap alg' [1..20]
    where
        alg' a    | a < 5 = a
            | True  = alg'(a-1) * alg(a-2) * alg(a-3) * alg(a-4)
propAlgT :: Bool
propAlgT = fmap L5.algT [1..20] == fmap L5.alg [1..20]

propSumSquare :: Positive Int -> Bool
propSumSquare (Positive n) = sumSquare n  == mySum n
    where 
        mySum x' = foldl (\x y -> x + y*y) 0 [0..x']


propSplit :: [Int] -> Bool
propSplit xs = (L5.split xs) == (mySplit xs)
    where
        mySplit = foldr (\x (as,bs) -> (x:bs, as)) ([],[])

propMerge :: ([Int], [Int]) -> Bool
propMerge xs = (L5.merge xs) == (myMerge xs)
    where
        myMerge = uncurry (foldr f id)
        --f :: a -> ([a] -> [a]) -> ([a] -> [a])
        f x h ys
            | []     <- ys = x:(h ys)
            | (z:zs) <- ys = x:z:(h zs)
            | otherwise    = undefined {- does not happen -}



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
    putStrLn "\nalg:"
    quickCheckFail propAlg

    putStrLn "\nalgT:"
    quickCheckFail propAlgT

    putStrLn "\nsplit:"
    quickCheckFail propSplit

    putStrLn "\nmerge:"
    quickCheckFail propMerge


