----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Homework03.Golf where

    import Data.List
    import Data.Maybe

    ----------------------------------------------------------------------
    -- Exercise 1
    ----------------------------------------------------------------------

    -- |
    --
    -- >>> skips "ABCD"
    -- ["ABCD","BD","C","D"]
    -- >>> skips "hello!"
    -- ["hello!","el!","l!","l","o","!"]
    -- >>> skips [1]
    -- [[1]]
    -- >>> skips [True, False]
    -- [[True,False],[False]]
    -- >>> skips []
    -- []

    skip :: Int -> [a] -> [a]
    skip n l
        | n < length l = ((drop n l) !! 0) : skip n (drop (n+1) l)
        | otherwise = []

    skips :: [a] -> [[a]]
    skips l = map f [0..(length l)-1] where f n = skip n l

    ----------------------------------------------------------------------
    -- Exercise 2
    ----------------------------------------------------------------------

    -- |
    --
    -- >>> localMaxima [2,9,5,6,1]
    -- [9,6]
    -- >>> localMaxima [2,3,4,1,5]
    -- [4]
    -- >>> localMaxima [1,2,3,4,5]
    -- []

    localMaxima :: [Integer] -> [Integer]
    localMaxima (a:b:c:l)
        | a < b && b > c = b:(localMaxima (c:l))
        | otherwise      = localMaxima (b:c:l)
    localMaxima _ = []

    ----------------------------------------------------------------------
    -- Exercise 3
    ----------------------------------------------------------------------

    -- |
    --
    -- >>> putStr (histogram [1,1,1,5])
    --  *
    --  *
    --  *   *
    -- ==========
    -- 0123456789
    -- >>> putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
    --     *
    --     *
    --     * *
    --  ******  *
    -- ==========
    -- 0123456789

    histogram :: [Integer] -> String
    histogram l = generateCounters (counter l)

    generateCounters :: [Integer] -> String
    generateCounters l
        | max_ == 0 = "==========\n0123456789\n"
        | otherwise = generateRow indices ++ generateCounters next_
        where max_ = maximum l
              indices = elemIndices max_ l
              getVal :: Int -> Integer
              getVal x
                  | elemIndex x indices == Nothing = l !! x
                  | otherwise = (l !! x) - 1
              next_ = [getVal i | i <- [0..9]]

    generateRow :: [Int] -> String
    generateRow n = [generatePixel n i | i <- [0..9]] ++ "\n"

    generatePixel :: [Int] -> Int -> Char
    generatePixel l x
        | elemIndex x l == Nothing = ' '
        | otherwise = '*'

    counter :: [Integer] -> [Integer]
    counter [] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    counter (x:l) = take n c ++ [(c !! n) + 1] ++ drop (n + 1) c
        where c = counter(l)
              n = fromIntegral x
