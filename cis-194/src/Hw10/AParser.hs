module Hw10.AParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  -- fmap :: (a -> b) -> f a -> f b
  -- Parser a = Parser {runParser String -> Maybe (a, String)}
  fmap f (Parser p) = Parser p'
    where p' = fmap (first f) . p

instance Applicative Parser where
  -- pure a -> f a
  pure a = Parser (\str -> Just (a, str))

  -- <*> :: f (a -> b) -> f a -> f b
  (Parser p1) <*> (Parser p2) = Parser p
    where p str = case p1 str of
                    Nothing -> Nothing
                    Just (func, rest) -> fmap (first func) . p2 $ rest

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = list <$> posInt <*> char ' ' <*> posInt
  where list a _ b = [a, b]

instance Alternative Parser where
  -- empty :: f a
  empty = Parser (const Nothing)
  -- <|> :: f a -> f a -> f a
  (Parser p1) <|> (Parser p2) = Parser p
    where p str = p1 str <|> p2 str

intOrUppercase :: Parser ()
intOrUppercase = parseUpper <|> parseInt
  where parseUpper = void $ satisfy isUpper
        parseInt = void posInt
