module Hw4.Hw4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f x = if even x then
                x `div` 2
                else
                3 * x + 1

data TreeH a = LeafH | NodeH Integer (TreeH a) a (TreeH a)
    deriving (Show, Eq)

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node left _ right) = max (treeHeight left) (treeHeight right) + 1

insert :: a -> Tree a -> Tree a
insert value Leaf = Node Leaf value Leaf
insert value (Node left v right)
    | treeHeight left < treeHeight right = Node (insert value left) v right
    | otherwise                          = Node left v (insert value right)

toTreeH :: Tree a -> TreeH a
toTreeH Leaf = LeafH
toTreeH tree@(Node left value right) = NodeH (treeHeight tree) (toTreeH left) value (toTreeH right)

foldTree :: [a] -> TreeH a
foldTree = toTreeH . foldr insert Leaf

xor :: [Bool] -> Bool
-- xor = odd . sum . map (\x -> if x then 1 else 0)
xor = odd . foldr ((+) . (\x -> if x then 1 else 0)) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (b -> a -> a) -> a -> [b] -> a
myFoldl f base xs = foldr f base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) . filter (`notElem` crossed) $ [1..n]
    where crossed = [i + j + 2*i*j | j <- [1..n - 1], i <- [1..j]]
