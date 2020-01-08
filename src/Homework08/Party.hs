----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Homework08.Party where

import Homework08.Employee
import Data.List
import Data.Tree
import Data.Semigroup

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL list fun) = GL (list++[employee]) (fun + (empFun employee)) 

instance Semigroup GuestList where
    (GL l1 f1) <> (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend = (<>) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 > l2 then l1 else l2


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold func (Node value list) = func value (map (treeFold func) list)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob list = (glCons bob (mconcat (map snd list)), mconcat (map (uncurry moreFun) list))


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel tree

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = do
    content <- readFile "Homework08/company.txt"
    let GL result fun = maxFun (read content :: Tree Employee)
    putStr $ "Total Fun: " ++ (show fun) ++ "\n"
    putStr $ unlines $ sort $ map empName result
