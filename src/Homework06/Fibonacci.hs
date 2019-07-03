----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 06
--
----------------------------------------------------------------------

module Homework06.Fibonacci where
import Data.List

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = map fst (iterate (\(x,y) -> (y, x+y)) (0,1))


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons n s) = n:(streamToList s) 

instance Show a => Show (Stream a) where
    show = show . take 100 . streamToList

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x) 

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 s1) s2 = Cons x1 $ interleaveStreams s2 s1

ruler :: Stream Integer
ruler = foldr1 interleaveStreams $ map streamRepeat [0..]

-- 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3


----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

x :: Stream Integer
x = undefined


----------------------------------------------------------------------
-- Exercise 7 (Optional)
----------------------------------------------------------------------

fib4 :: Integer -> Integer
fib4 = undefined
