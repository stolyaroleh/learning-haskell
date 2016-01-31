module Chapter3 where

import Data.Function
import Data.List

data FailableDouble = Failure
                    | OK Double
     deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0.0 = Failure
safeDiv x y   = OK (x / y)

unsafeDiv :: Double -> Double -> Double
unsafeDiv x y = if y == 0
                then error "Division by zero"
                else x / y

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0.0
failureToZero (OK d)  = d


data List a = Empty
              | Cons a (List a)
     deriving Show

toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Empty       = []

fromList :: [a] -> List a
fromList = foldr Cons Empty

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
     deriving Show

treeHeight :: Tree a -> Integer
treeHeight (Node _ left right) = 1 + max (heightOrZero left) (heightOrZero right)
    where heightOrZero Nothing  = 0
          heightOrZero (Just t) = treeHeight t

len :: [a] -> Integer
len = foldr ((+) . const 1) 0

mean :: [Integer] -> Double
mean [] = 0.0
mean l = s / len'
    where s    = fromIntegral $ sum l
          len' = fromIntegral $ len l

palindrome :: [a] -> [a]
palindrome l = l ++ reverse l

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

quickSortBy :: Ord o => [a] -> (a -> o) -> [a]
quickSortBy [] _       = []
quickSortBy (x:xs) key = quickSortBy smaller key ++ [x] ++ quickSortBy larger key
    where smaller = [y | y <- xs, key y <= key x]
          larger  = [y | y <- xs, key y >  key x]

interperse :: [a] -> [[a]] -> [a]
interperse sep (x:y:xs) = x ++ sep ++ interperse sep (y:xs)
interperse _ [x] = x
interperse _ [] = []

data Direction = Direction {
     initial :: Double,
     delta   :: Double
}

instance Show Direction where
    show (Direction i d) = "Direction: initial " ++ show (i / pi) ++ " delta " ++ show (d / pi)

isRight :: Direction -> Bool
isRight (Direction _ d) = d < 0.0

isLeft :: Direction -> Bool
isLeft (Direction _ d) = d > 0.0

isStraight :: Direction -> Bool
isStraight (Direction _ d) = d == 0.0

type Point2 = (Double, Double)
type Vec2 = (Double, Double)

vec2 :: Point2 -> Point2 -> Vec2
vec2 (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

magnitude :: Vec2 -> Double
magnitude (x, y) = sqrt (x * x + y * y)

dot :: Vec2 -> Vec2 -> Double
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

angle :: Vec2 -> Vec2 -> Double
angle v1@(x1, y1) v2@(x2, y2)
    | magnitude v1 == 0 || magnitude v2 == 0 = 0.0
    | otherwise = atan2 perpdot (dot v1 v2)
        where perpdot = x1 * y2 - y1 * x2

turn :: Point2 -> Point2 -> Point2 -> Direction
turn a b c = Direction start (end - start)
    where start = angle (vec2 a b) (1, 0)
          end   = angle (vec2 b c) (1, 0)

testHullPoints :: [(Double, Double)]
testHullPoints = [(0.0, 0.0), (1.0, 0.0), (2.0, 0.0),
                  (0.0, 1.0), (1.0, 1.0), (2.0, 1.0),
                  (0.0, 2.0), (1.0, 2.0), (2.0, 2.0)]

pivot :: [Point2] -> Point2
pivot = minimumBy (\(x, y) -> compare (y, x))

angleWith :: Point2 -> Point2 -> Double
angleWith piv p = angle (vec2 piv p) (1, 0)

sortedHullPoints :: Point2 -> [Point2] -> [Point2]
sortedHullPoints piv points = sortBy (compare `on` angleWith piv) pointsWithoutPivot
    where pointsWithoutPivot = delete piv (nub points)

convexHull :: [Point2] -> [Point2]
convexHull points = p : hullify (p:hullPoints)
    where p = pivot points
          hullPoints = sortedHullPoints p points
          hullify (a:b:c:rest) = if not $ isRight (turn a b c)
                                 then b : hullify (b:c:rest)
                                 else hullify (b:c:rest)
          hullify _ = []
