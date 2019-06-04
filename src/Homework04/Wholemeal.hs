----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Homework04.Wholemeal where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- |
--
-- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
-- True
-- >>> fun1 [1,2,3] /= fun1' [1,2,3]
-- False
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 15 /= fun2' 15
-- False

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2Helper :: Integer -> Integer
fun2Helper n
  | even    n = n `div` 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate fun2Helper

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insertValue :: Tree a -> (Int, a) -> Tree a
insertValue Leaf (_, v) = Node 0 Leaf v Leaf
insertValue (Node _ l n r) (i, v)
  | even i    = Node (x+1) l n node
  | otherwise = Node (x+1) node n r
    where subtree = if even i then r else l
          node = insertValue subtree (i `div` 2, v)
          Node x _ _ _ = node    

foldTree :: [a] -> Tree a
foldTree = foldl insertValue Leaf . zip [1..]


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

xor' :: Bool -> Bool -> Bool
xor' False False = False
xor' a b = a /= b

xor :: [Bool] -> Bool
xor = foldl xor' False

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

transform' :: (a -> b) -> a -> [b] -> [b]
transform' f x l = f x : l  

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (transform' f) []

-- Optional

-- reverse :: [a] -> [a]
-- reverse []   = []
-- reverse x:xs = xs ++ [x]


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x*2+1) (filter validInt [1..n])

validInt :: Integer -> Bool
validInt n = all (\v -> v /= n) (map operation (combine' x x))
    where x = n `div` 2

operation :: (Integer, Integer) -> Integer
operation (a, b) = a + b + 2 * a * b 

combine' :: Integer -> Integer -> [(Integer, Integer)]
combine' xs ys = [(x,y) | x <- [1..xs], y <- [1..ys], x <= y]