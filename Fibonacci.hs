{-# LANGUAGE FlexibleInstances #-}

import Data.Bits

--1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--2
fibs2 :: [Integer]
--fibs2 = [0, 1] ++ [fibs2!!(n - 1) + fibs2!!(n - 2) | n <- [2..]]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

--3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

--4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons x xs) = Cons (func x) (streamMap func xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = Cons seed (streamFromSeed rule (rule seed))

--5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleavesStreams :: Stream a -> Stream a -> Stream a
interleavesStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleavesStreams xs ys))

ruler :: Stream Integer
ruler = interleavesStreams (streamRepeat 0) (streamMap (\x -> round (logBase 2 $ fromIntegral $ x .&. (-x :: Integer) )) (streamFromSeed (+ 2) 2))

--6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
	fromInteger y = Cons y (streamRepeat 0)
	negate (Cons y ys) = Cons (-y) (negate ys)
	(+) (Cons y ys) (Cons z zs) = Cons (y + z) ((+) ys zs)
	(*) (Cons y ys) (Cons z zs) = Cons (y * z) ((+) (streamMap (* y) zs) ((*) ys (Cons z zs)))

instance Fractional (Stream Integer) where
	(/) (Cons y ys) (Cons z zs) = Cons (y `div` z) (streamMap (* (1 `div` z)) (ys - ((*) ((/) (Cons y ys) (Cons z zs)) zs)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)