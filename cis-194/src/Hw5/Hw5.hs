{-# LANGUAGE FlexibleInstances #-}
module Hw5.Hw5 where

import Hw5.StackVM
import Hw5.Parser
import qualified Data.Map as M

data ExprT = LitT Integer
           | AddT ExprT ExprT
           | MulT ExprT ExprT

eval :: ExprT -> Integer
eval (LitT x)   = x
eval (AddT a b) = eval a + eval b
eval (MulT a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp LitT AddT MulT s of
            Just expr -> Just $ eval expr
            otherwise -> Nothing

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr Integer where
    lit x   = x
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit x   = x > 0
    add a b = a || b
    mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr MinMax where
    lit   = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
    lit x   = Mod7 $ x `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Program where
    lit x   = [PushI x]
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

data VarExprT = LitV Integer
              | AddV VarExprT VarExprT
              | MulV VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = LitV
    add = AddV
    mul = MulV

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x   m = Just x
    add a b m = case (a m, b m) of
                (Just x, Just y) -> Just (x + y)
                otherwise        -> Nothing
    mul a b m = case (a m, b m) of
                (Just x, Just y) -> Just (x * y)
                otherwise        -> Nothing

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
