{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 05
--
----------------------------------------------------------------------

module Homework05.Calc where

import Homework05.ExprT
import Homework05.Parser
import qualified Homework05.StackVM as VM
import qualified Data.Map as M

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20
-- True

eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

eval' :: Maybe ExprT -> Maybe Integer
eval' Nothing  = Nothing
eval' (Just a) = Just (eval a)
----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr = eval' . parseExp Lit Add Mul


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

reify :: ExprT -> ExprT
reify = id

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

newtype MinMax  = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7    = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add = max 
    mul = min

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = lit (a + b)
    mul (Mod7 a) (Mod7 b) = lit (a * b)

----------------------------------------------------------------------
-- Exercise 5 (do this OR exercise 6)
----------------------------------------------------------------------

compile :: String -> Maybe VM.Program
compile = undefined


----------------------------------------------------------------------
-- Exercise 6 (do this OR exercise 5)
----------------------------------------------------------------------

-- |
--
-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
