-- | Haskell Lab 4 exercises
--
-- This set of exercises is straightforward -- implement Selection Sort,
-- Insertion Sort, and Bubble Sort, which you have seen during the lecture.
--
-- However, there is slight modification you need to account for. Your version
-- of sort function is a proivided with a list of arbitrary elements, not only
-- integers, and an accessor function that takes an element of the list and
-- returs a key that is used to sort the list.
--
-- Assume a list of pairs:
--
-- > [(4,'a'), (0, 'b'), (3, 'c')]
--
-- and an accessor function 'fst'
--
-- > fst :: (a, b) -> a
-- > fst (x, _) = x
--
-- Then your implementation should behave in the following way:
--
-- >>> selSortWith fst [(4,'a'), (0, 'b'), (3, 'c')]
-- [(0,'b'), (3, 'c'), (4, 'a')
--
--
module Lab4 (
      selectionSortWith
    , insertionSortWith
    , bubbleSortWith
) where

-- | Implement a variant of Selection Sort that uses an accesor function
--
selectionSortWith :: (a -> Int) -> [a] -> [a]
selectionSortWith = undefined

-- | Implement a variant Insertion Sort that uses an accesor function
--
insertionSortWith :: (a -> Int) -> [a] -> [a]
insertionSortWith = undefined

-- | Implement a variant Bubble Sort that uses an accesor function
--
bubbleSortWith :: (a -> Int) -> [a] -> [a]
bubbleSortWith = undefined





