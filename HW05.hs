module HW05 where
import Ring
import Parser
import Data.Maybe


--1
intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) && (parseRing "1 + 2 * 5" == Just (11 :: Integer)) && (addId == (0 :: Integer))

--2
data Mod5 = MkMod Integer
	deriving (Show, Eq)

instance Ring Mod5 where
  addId  = MkMod 0
  addInv = (\ (MkMod x) -> MkMod (negate x))
  mulId  = MkMod 1

  add (MkMod x) (MkMod y) = MkMod ((x + y) `mod` 5)
  mul (MkMod x) (MkMod y) = MkMod ((x * y) `mod` 5)

mod5ParsingWorks :: Bool
mod5ParsingWorks = (parseRing "1 + 2 * 5" == Just (MkMod 11 :: Mod5)) && (addId == (MkMod 0 :: Mod5))