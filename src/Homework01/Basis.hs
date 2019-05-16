----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 01
--
----------------------------------------------------------------------

module Homework01.Basis where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]


toDigitsRev :: [Integer] -> [Integer]
toDigitsRev [] = []
toDigitsRev (e:l) = toDigitsRev(l) ++ [e]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:l) = x:y*2:doubleEveryOther(l)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> sumDigits [16,7,12,5]
-- 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:l) = sum(toDigits x) + sumDigits(l)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate n = 0 == sumDigits(doubleEveryOther(toDigitsRev(toDigits(n)))) `mod` 10

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

-- |
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
-- >>> hanoi 3 "a" "b" "c"
-- [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- Hanoi Towers with 4 Pegs

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, b)]
hanoi4 n a b c d = hanoi4 (n-2) a d b c ++ [(a, c), (a, b), (b, c)] ++ hanoi4 (n-2) d b a c
