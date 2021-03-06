{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module Homework10.AParser where

-- base
import Control.Applicative
import Data.Char


newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g (Parser f) = Parser $ (fmap $ first g) . f


first :: (a -> b) -> (a, c) -> (b, c)
first g (x, s) = (g x, s) 


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> Just (x , s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f1 <*> p2 = Parser $ \s1 ->
    case f1 s1 of
      Nothing      -> Nothing
      Just (x, s2) -> runParser (x <$> p2) s2

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'


-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
intPair = join <$> posInt <*> char ' ' <*> posInt
  where join a _ b = [a, b]

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f1 <|> Parser f2 = Parser $ \s ->
    case f1 s of
      Nothing -> f2 s
      Just x  -> Just x 


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intOrUppercase :: Parser ()
intOrUppercase = (f <$> posInt) <|> (f <$> satisfy isUpper)
  where f _ = ()