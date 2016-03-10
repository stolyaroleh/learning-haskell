{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hw12.Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- Rand StdGen === MRand
type MRand = Rand StdGen

die :: MRand DieValue
die = getRandom

dice :: Int -> MRand [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

instance Show Battlefield where
         show bf = "Attackers " ++ show (attackers bf) ++ " \t" ++
                   "Defenders " ++ show (defenders bf)

chooseTroops :: Battlefield -> (Army, Army)
chooseTroops bf = (a, d)
               where a = max 0 $ min (attackers bf - 1) 3
                     d = max 0 $ min (defenders bf) 2

roll :: (Army, Army) -> MRand ([DieValue], [DieValue])
roll (a, d) = do
    da <- dice a
    dd <- dice d
    return (da, dd)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

losses :: ([DieValue], [DieValue]) -> (Army, Army)
losses (aRolls, dRolls) = let l = zipWith (>) (sortDesc aRolls) (sortDesc dRolls)
                              dDead = length . filter id $ l
                              aDead = length l - dDead
                          in (aDead, dDead)

battle :: Battlefield -> MRand Battlefield
battle bf = do
    rolls <- roll . chooseTroops $ bf
    return $ let (aDead, dDead) = losses rolls
             in Battlefield (attackers bf - aDead) (defenders bf - dDead)

invade :: Battlefield -> MRand Battlefield
invade bf
  | attackers bf < 2 || defenders bf < 1 = return bf
  | otherwise                            = battle bf >>= invade

success :: [Battlefield] -> Double
success bfs = fromIntegral wins / fromIntegral (length bfs)
        where wins = length . filter ((== 0) . defenders) $ bfs

successProb :: Battlefield -> MRand Double
successProb bf = liftM success (invasions bf)
    where invasions = sequence . replicateM 1000 invade :: Battlefield -> MRand [Battlefield]
