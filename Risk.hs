{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
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

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
	deriving (Show, Read, Eq)

testBattlefield :: Battlefield
testBattlefield = Battlefield 7 4

testDV :: DieValue
testDV = DV 1

testDV2 :: DieValue
testDV2 = DV 3

testDV4 :: DieValue
testDV4 = DV 6

testDVList1 :: [Int]
testDVList1 = [6, 6, 3]

testDVList2 :: [Int]
testDVList2 = [5, 4]
--2
battle :: Battlefield -> Rand StdGen Battlefield
battle b = replicateM (uncurry (+) ps) die >>= \rands ->
			return (armiesToBattlefield (compareRolls (getDice ps (map unDV rands)) (attackers b, defenders b))) where
				ps = getArmies b

getArmies :: Battlefield -> (Army, Army)
getArmies battle = ((min 3 (max ((attackers battle) - 1) 0)), (min 2 (defenders battle)))

getDice :: (Army, Army) -> [Int] -> ([Int], [Int])
getDice (att, def) rolls = (sortRoll (take att rolls), sortRoll (drop def rolls))

sortRoll :: [Int] -> [Int]
sortRoll values = reverse(sort values)

compareRolls :: ([Int], [Int]) -> (Army, Army) -> (Army, Army)
compareRolls (attackers, defenders) (totalattackers, totaldefenders)
	| (attackers == []) = (totalattackers, totaldefenders)
	| (defenders == []) = (totalattackers, totaldefenders)
	| (head attackers) > (head defenders) = concatTuples (0, -1) (compareRolls (tail attackers, tail defenders) (totalattackers, totaldefenders))
	| otherwise = concatTuples (-1, 0) (compareRolls (tail attackers, tail defenders) (totalattackers, totaldefenders))


concatTuples :: (Army, Army) -> (Army, Army) -> (Army, Army)
concatTuples (a, b) (c, d) = (a+c, b+d)

armiesToBattlefield :: (Army, Army) -> Battlefield
armiesToBattlefield (army1, army2) = Battlefield army1 army2

--3
invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= \bf ->
	if attackers bf < 2 || defenders bf <= 0
		then return bf
		else invade bf

--4
success :: Battlefield -> Bool
success = (== 0) . defenders

successProb :: Battlefield -> Rand StdGen Double
successProb = liftM (\xs -> fromIntegral (length xs) / 1000.0) . liftM (filter success) . replicateM 1000 . invade

(|>) :: (b -> a) -> (a -> c) -> b -> c
(f |> g) x = g (f x)

main :: IO()
main = do
		b <- evalRandIO (battle (Battlefield 4 1))
		print b