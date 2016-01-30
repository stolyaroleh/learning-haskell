{-# LANGUAGE FlexibleInstances #-}
module Hw7.JoinListBuffer where

import Hw7.Sized
import Hw7.JoinList
import Hw7.Scrabble
import Hw7.Buffer

type JoinListBuffer = (JoinList (Score, Size) String)

instance Buffer JoinListBuffer where
    toString = unlines . jlToList
    fromString = foldr ((+++) . toJL) Empty . lines
        where toJL s = Single (scoreString s, 1) s
    line = indexJ
    replaceLine i s jl
        | i > numLines jl = jl
        | i < 0           = jl
        | otherwise       = takeJ i jl +++ fromString s +++ dropJ (i + 1) jl
    numLines = getSize . snd . tag
    value    = getScore . fst . tag
