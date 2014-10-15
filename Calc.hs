{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExprT
import Parser
import StackVM

--1
eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

--2
evalStr :: String -> Maybe Integer
evalStr string = evalMaybe(parseExp ExprT.Lit ExprT.Add ExprT.Mul string)

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just a) = Just (eval a)

--3
class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    mul a b = ExprT.Mul a b
    add a b = ExprT.Add a b
    lit a = ExprT.Lit a

--4
reify :: Mod7 -> Mod7
reify = id

instance Expr Integer where
    mul a b = a * b
    add a b = a + b
    lit a = a

instance Expr Bool where
    mul a b = a && b
    add a b = a || b
    lit a
        | (a > 0) = True
        | otherwise = False

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    add (MinMax a) (MinMax b) = MinMax (max a b)
    lit a = MinMax a

instance Expr Mod7 where
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    lit a = Mod7 (a `mod` 7)

--5
instance Expr Program where
    mul a b = a ++ b ++ [StackVM.Mul]
    add a b = a ++ b ++ [StackVM.Add]
    lit a = [StackVM.PushI a]

compile :: String -> Maybe Program
compile string = parseExp lit add mul string :: Maybe Program

