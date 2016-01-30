module Hw8.Party where

import Hw8.Employee
import Data.List
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)

emptyGuestList :: GuestList
emptyGuestList = mempty

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l f) (GL l' f') = GL (l ++ l') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ f) l'@(GL _ f') = if f >= f' then l
                                             else l'

-- no accumulator here, for simplicity
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root children) = f root $ map (treeFold f) children

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss employees = (withBoss, withoutBoss)
    where withBoss    = GL [boss] (empFun boss)
          withoutBoss = foldr combineBestGuests emptyGuestList employees
            where combineBestGuests = (<>) . uncurry moreFun

maxBy :: Ord r => (a -> r) -> a -> a -> a
maxBy f a b = if f a >= f b then a else b

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

maxFun :: Tree Employee -> GuestList
maxFun = uncurry (maxBy getFun) . treeFold nextLevel

formatGL :: GuestList -> String
formatGL (GL l fun) = unlines $ ("Total fun: " ++ show fun) : sortedNames
    where sortedNames = sort . map empName $ l

main :: IO()
main = do
    contents <- readFile "src/Hw8/company.txt"
    putStrLn . formatGL . maxFun . read $ contents