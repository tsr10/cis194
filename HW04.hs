module HW04 where

{-# OPTIONS_GHC -Wall #-}

import Data.List
import Data.Maybe
import Data.Char

--1
ex1 :: a -> b -> b
ex1 _ y = y

--2
ex2 :: a -> a -> a
ex2 x y = x

--3
ex3 :: Int -> a -> a
ex3 _ x = x

--4
ex4 :: Bool -> a -> a -> a
ex4 x y z
    | (x == True) = y
    | otherwise = z

--5
ex5 :: Bool -> Bool
ex5 x
    | (x ==True) = True
    | otherwise = False

--6
--ex6 :: (a -> a) -> a
--ex6 x = (x 1)

--7
ex7 :: (a -> a) -> a -> a
ex7 x y = x y

--8
ex8 :: [a] -> [a]
ex8 x = x

--9
ex9 :: (a -> b) -> [a] -> [b]
ex9 x y = map x y

--10
ex10 :: Maybe a -> a
ex10 x = fromJust x

--11
ex11 :: a -> Maybe a
ex11 x = Just x

--12
ex12 :: Maybe a -> Maybe a
ex12 x = x

--14
allCaps :: [String] -> Bool
allCaps string = all(\x -> isUpper(head x)) string

--15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace string = dropWhileEnd isSpace string

--16
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

firstLetters :: [String] -> [Char]
firstLetters string = catMaybes(map safeHead string)

--17
asList :: [String] -> String
asList string = intercalate "," string