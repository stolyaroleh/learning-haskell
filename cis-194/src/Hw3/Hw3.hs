module Hw3.Hw3 where

import Data.List

skip :: [a] -> [[a]]
skip l = [[l !! n | n <- [s - 1, 2*s - 1..length l - 1]] | s <- [1..length l]]

localMaxima :: [Integer] -> [Integer]
localMaxima l = [b | (a, b, c) <- zip3 l (drop 1 l) (drop 2 l), b > a && b > c]

histogram :: [Integer] -> String
histogram l = unlines . reverse $ ["123456789"] ++ ["========="] ++ plot (filter (\x -> x > 0 && x < 10) l)
    where plot [] = [""]
          plot l  = [if n `elem` l then '*' else ' ' | n <- [1..9]] : plot rest
            where rest = foldl (\l f -> f l) l [delete n | n <- [1..9]]