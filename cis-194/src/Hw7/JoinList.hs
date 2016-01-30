module Hw7.JoinList where

import Hw7.Sized
import Hw7.Scrabble
import Hw7.Buffer
import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) a b = Append (tag a <> tag b) a b

len :: Sized a => a -> Int
len = getSize . size

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
    | i == 0    = Just a
    | otherwise = Nothing
indexJ i (Append m left right)
    | i < 0     = Nothing
    | i < len m = let lsize = len $ tag left in
                  if i < lsize then indexJ i left
                               else indexJ (i - lsize) right
    | otherwise  = Nothing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a)   = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ _ Empty = Empty
dropJ i l@(Single _ _)
    | i < 1     = l
    | otherwise = Empty
dropJ i l@(Append m left right)
    | i < 1      = l
    | i >= len m = Empty
    | otherwise  = let lsize = len $ tag left in
                   if i < lsize then dropJ i left +++ right
                                else dropJ (i - lsize) right

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ _ Empty = Empty
takeJ i l@(Single _ _)
    | i < 1     = Empty
    | otherwise = l
takeJ i l@(Append m left right)
    | i < 1      = Empty
    | i >= len m = l
    | otherwise  = let lsize = len $ tag left in
                   if i < lsize then takeJ i left
                                else left +++ takeJ (i - lsize) right

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
