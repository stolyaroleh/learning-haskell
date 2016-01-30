{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hw7.Scrabble where

import Hw7.Sized
import Data.Monoid
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

scores :: M.Map Char Score
scores = M.fromList [('a', 1), ('b', 3), ('c', 3), ('d', 2),
                     ('e', 1), ('f', 4), ('g', 2), ('h', 4),
                     ('i', 1), ('j', 8), ('k', 5), ('l', 1),
                     ('m', 3), ('n', 1), ('o', 1), ('p', 3),
                     ('q', 10), ('r', 1), ('s', 1), ('t', 1),
                     ('u', 1), ('v', 4), ('w', 4), ('x', 8),
                     ('y', 4), ('z', 10)]

score :: Char -> Score
score c = fromMaybe 0 (M.lookup c scores)

scoreString :: String -> Score
scoreString = foldr ((+) . score) 0

getScore :: Score -> Int
getScore (Score s) = s
