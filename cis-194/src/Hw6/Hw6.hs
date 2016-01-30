{-# LANGUAGE FlexibleInstances #-}
module Hw6.Hw6 where

import Data.List

stupidFib :: Integer -> Integer
stupidFib 0 = 0
stupidFib 1 = 1
stupidFib n
    | n > 0     = stupidFib (n - 1) + stupidFib (n - 2)
    | otherwise = undefined

fib :: Integer -> Integer
fib n = head . genericDrop n $ fibs

stupidFibs = map stupidFib [0..]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

instance Show a => Show (Stream a) where
    show stream = show $ take 20 . streamToList $ stream

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a sa) = Cons (f a) (streamMap f sa)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f value = Cons value (streamFromSeed f (f value))

zeros :: Stream Integer
zeros = streamRepeat 0

-- 0 1 0 0 0 0 ...
x :: Stream Integer
x = Cons 0 (Cons 1 zeros)

instance Num (Stream Integer) where
    fromInteger n = Cons n zeros

    (+) (Cons a a') (Cons b b') = Cons (a + b) (a' + b')
    (-) (Cons a a') (Cons b b') = Cons (a - b) (a' - b')
    (*) (Cons a a') sb@(Cons b b') = Cons (a * b) (streamMap (a*) b' + a' * sb)

    signum _ = 1
    abs = id

instance Fractional (Stream Integer) where
    (/) (Cons a a') (Cons b b') = q
            where q = Cons (a `div` b) (streamMap (`div` b) (a' - q * b'))

    fromRational _ = zeros

polyFibs :: Stream Integer
polyFibs = x / (1 - x - x^2)

data Matrix2x2 = M Integer Integer Integer Integer
    deriving (Eq, Show)

instance Num Matrix2x2 where
    (*) (M a b c d) (M a' b' c' d') = M na nb nc nd
        where na = a * a' + b * c'
              nb = a * b' + b * d'
              nc = c * a' + d * c'
              nd = c * b' + d * d'

    -- not needed for this assignment
    (+) (M a b c d) (M a' b' c' d') = M (a + a') (b + b') (c + c') (d + d')
    (-) (M a b c d) (M a' b' c' d') = M (a - a') (b - b') (c - c') (d - d')
    fromInteger i      = M i i i i
    signum             = const 1
    abs                = id

fibMatrix :: Integer -> Integer
fibMatrix 0 = 0
fibMatrix n = takeF (m^n)
    where takeF (M a b c d) = b
          m = M 1 1 1 0
