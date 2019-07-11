----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Homework08.Party where

import Homework08.Employee
import Data.Tree

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL list fun) = GL (list++[employee]) (fun + (empFun employee)) 

instance Monoid GuestList where
    mempty = GL [] 0
    (GL l1 f1) `mappend` (GL l2 f2) = GL (l1 ++ l2) (f1 + f2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 > l2 then l1 else l2


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

treeFold :: (a -> [Tree a] -> b) -> Tree a -> b
treeFold func (Node value list) = func value list

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob list = (glCons bob (mconcat (map snd list)), mconcat (map fst list))


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFunTuple :: (GuestList, GuestList) -> GuestList
maxFunTuple (a, b) = moreFun a b

processNode :: Tree Employee -> (GuestList, GuestList)
processNode (Node employee list) = nextLevel employee $ map processNode list

maxFun :: Tree Employee -> GuestList
maxFun tree = maxFunTuple $ processNode tree


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = undefined
