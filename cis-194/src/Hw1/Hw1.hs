{-# LANGUAGE ParallelListComp #-}

module Hw1.Hw1 where

-- Credit Card Validation
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x < 0     = toDigitsRev (-x)
    | x < 10    = [x]
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = [f x | x <- l | f <- cycle functions]
    where functions = if even (length l) then [(*2), id] else [id, (*2)]

sumDigits :: [Integer] -> Integer
sumDigits l = sum [sum (toDigits x) | x <- l]

validate :: Integer -> Bool
validate x = (magicSum `mod` 10) == 0
    where magicSum = sumDigits . doubleEveryOther . toDigits $ x

-- Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = hanoiN n [a, b, c]

hanoiN :: Integer -> [Peg] -> [Move]
hanoiN 0 _ = []
hanoiN 1 (p1 : p2 : rest) = [(p1, p2)]
hanoiN n (p1 : p2 : p3 : rest) =
    hanoiN k (p1 : p3 : p2 : rest) ++
    hanoiN (n - k) (p1 : p2 : rest) ++
    hanoiN k (p3 : p2 : p1 : rest)
    where k = if null rest then n - 1 else n `quot` 2
hanoiN _ _ = error "Invalid configuration"