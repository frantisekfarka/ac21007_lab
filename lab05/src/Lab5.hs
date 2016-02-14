-- | = Haskell Lab 5 exercises
--
-- This set of exercises is straightforward -- implement all the undefined
-- functions in this files as asked in comments
--
--
--
-- == Alg and AlgT from Friday test
--
-- One of the tasks is to implement algorithm __ALG__ that (or similar) was given
-- in last Friday test:
--
-- > Algorithm alg(int n)
-- > {if n = 0
-- >      return 1;
-- >   if n < 5
-- >      return n;
-- > else 
-- >   return alg(n-1) * alg(n-2) * alg(n-3) * alg(n-4);}
--
-- Once you implement functions 'alg' and 'algT' try computing alg(20) or
-- alg(50) for both recursive and tail-recursive versions. What do you notice?
--
module Lab5 (
      alg
    , algT
    , sumSquare
    , split
    , merge
) where

-- | Write a recursive function computing alg.
alg :: Integer -> Integer
alg = undefined


-- | Write a tail-recursive version (called algT) of the above algorithm. 
algT :: Integer -> Integer
algT = undefined


-- | Using foldr or foldl, whichever is appropriate, given a positive number n 
-- implement a function that returns the sum of squares of the natural numbers
-- 1 to n. You may need to use map, see previous lectures.
-- 
-- Example of evaluation:
--
-- > sumSqure 2
-- >>> 5
--
sumSquare :: Int -> Int
sumSquare = undefined


-- | Define a function "split" using foldl or foldr such that
-- this function returns lists of elements with odd indicies and with even
-- indicies, e.g.:
--
-- > split [1,2,3,4,5]
-- >>> ([1,3,5], [2,4])
--
split :: [a] -> ([a],[a])
split = undefined


-- | Define a function "merge" using foldl or foldr such that
-- this function takes two lists of elements and returns a new list interleaving
-- the two lists on input: 
-- 
-- > merge ([1,3,5], [2,4])
-- >>> [1,2,3,4,5]
--
merge :: ([a], [a]) -> [a]
merge = undefined




-- | Devise a representation for standard 52-card French deck
-- of cards. Fill in constructors of data types 'Suit' and 'Card'
-- such that following functions and class instances work without
-- any change (see the definitions further
--

{- The following part of this source file is commented out such that
 - previous parts compile even though the data types for Suits and Cards
 - are missing. Once you start with this task uncomment the rest of this
 - file (do not remember also delete the ending brace of this multiline 
 - comment at the end of this file).

data Suit = <<YOUR DEFINITION OF SUITS>>

data Card
    = <<YOUR DEFINITION OF CARDS>>


 
-- | Takes a suit and a number of card and creates the appropriate
-- card in our representation, the mapping of numbers to card ranks is
-- as follows: 
--
-- 1        -> Ace
-- 2..10    -> 2..10
-- 11       -> Jack
-- 12       -> Queen
-- 13       -> King
--
createCard :: Suit -> Int -> Card
createCard  suit    1       = Ace suit
createCard  suit    11      = Jack suit
createCard  suit    12      = Queen suit 
createCard  suit    13      = King suit
createCard  suit    rank    = if (rank > 1 && rank < 11)
                                then Card rank suit
                                else error "Wrong rank"

-- | We define a constant for ace of Spades:
aceOfSpades :: Card
aceOfSpades = createCard Spades 1

-- | We define an instance of typeclass 'Show' for our 'Suit'
instance Show Suit where
    show Hearts     = "Hearts"
    show Diamonds   = "Diamonds"
    show Clubs      = "Clubs"
    show Spades     = "Spades"


-- | And also an instance for our card representation. Note that we are
-- recursively using Show Suit instance (and also Show Int instance)
instance Show Card where
    show (Ace suit)         = "ace of " ++ show suit
    show (Card rank suit)   = show rank ++ " of " ++ show suit
    show (Jack suit)        = "jack of " ++ show suit
    show (Queen suit)       = "queen of " ++ show suit
    show (King suit)        = "king of " ++ show suit


-- | And finally, a pretty-printing function using our show:
ppFavoriteCard :: Card -> String
ppFavoriteCard c = "Your favourite card is: " ++ show c


THIS IS THE END OF THE COMMENT, DELETE HERE -}


