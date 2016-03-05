module Hw11.SExpr where

import Hw10.AParser
import Control.Applicative
import Data.Char

-- const id :: a -> b -> b
-- const    :: a -> b -> a

-- (*>) :: Applicative f => f a -> f b -> f b
-- (*>) a b = const id <$> a <*> b

-- (<*) :: Applicative f => f a -> f b -> f a
-- (<*) a b = const <$> a <*> b

-- sequenceA is already defined in Prelude
sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (f:fs) = (:) <$> f <*> sequenceA' fs

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA' . map f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA' . replicate n

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = Parser f
--   where f = recursiveRun []
--          where recursiveRun rs str = case runParser p str of
--                                      Nothing -> Just(reverse rs, str)
--                                      Just (r, rest) -> recursiveRun (r:rs) rest

zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

atom :: Parser Atom
atom = N <$> posInt <|> I <$> ident

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

trimSpaces :: Parser a -> Parser a
trimSpaces p = spaces *> p <* spaces

sexpr :: Parser SExpr
sexpr = trimSpaces $ A <$> atom <|> Comb <$> comb
      where comb = char '(' *> oneOrMore sexpr <* char ')'
