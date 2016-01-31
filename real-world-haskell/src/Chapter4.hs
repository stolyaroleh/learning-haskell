module Chapter4 where

import Data.Char
import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l  = Just . head $ l

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail l  = Just . tail $ l

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l  = Just . last $ l

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit l  = Just . tail $ l

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f l = let t = takeWhile f l
                    rest = dropWhile (not . f) . dropWhile f $ l in
                case t of
                [] ->     splitWith f rest
                _  -> t : splitWith f rest

interactWith :: (String -> String) -> String -> String -> IO()
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

firstWords :: (String -> String)
firstWords = unlines . firstWordsOrEmpty . lines
    where firstWordsOrEmpty = map (fromMaybe "" . safeHead . words)

transposeText :: (String -> String)
transposeText = unlines . transpose . lines

asIntFold :: String -> Int
asIntFold ""  = error "Empty string"
asIntFold "-" = error "Minus what?"
asIntFold (x:xs)
    | x == '-'  = - toInt xs
    | otherwise =   toInt (x:xs)
        where toInt = foldl step 0
              step :: Int -> Char -> Int
              step acc c
                 | not (isDigit c) = error $ c : " Is not a digit"
                 | signum acc * signum acc' < 0 = error "Overflow"
                 | otherwise = acc'
                where acc' = digitToInt c + (acc * 10)

type ErrorMessage = String

asIntEither :: String -> Either ErrorMessage Int
asIntEither ""  = Left "Empty string"
asIntEither "-" = Left "Minus what?"
asIntEither (x:xs)
    | x == '-'  = fmap negate . toInt $ xs
    | otherwise = toInt (x:xs)
        where toInt = foldl step (Right 0)
              step :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int
              step (Left err) _ = Left err
              step (Right acc) c
                 | not (isDigit c) = Left $ c : " Is not a digit"
                 | signum acc * signum acc' < 0 = Left "Overflow"
                 | otherwise = Right acc'
                where acc' = digitToInt c + (acc * 10)

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile' f xs
    | otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f = fst . foldl step ([], False)
    where (acc, done) `step` a
            | f a && not done = (a : acc, done)
            | otherwise = (acc, True)

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step []
    where step a ((x:xs):ls)
            | f a x     = (a:x:xs):ls
            | otherwise = [a]:(x:xs):ls
          step a _ = [[a]]

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f = foldr (\a acc -> f a || acc) False

cycle' :: [a] -> [a]
cycle' [] = error "Empty list"
cycle' xs = foldr (\_ acc -> xs ++ acc) [] [1..]

words' :: String -> [String]
words' = filter (/= "") . foldr step []
    where step x []
            | isSpace x = [[]]
            | otherwise = [[x]]
          step x (y:ys)
            | isSpace x = [] : (y:ys)
            | otherwise = (x:y):ys

unlines' :: [String] -> String
unlines' = foldr (\s acc -> s ++ '\n' : acc) ""

