{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JoinList where

import Buffer
import Data.Monoid
import Sized
import Scrabble
--1

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag(x) <> tag(y)) x y

--2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ value (Single _ y)
    | (value == 0) = Just y
    | otherwise = Nothing
indexJ value (Append _ l r)
    | (value < leftsize) = indexJ value l
    | (value > leftsize) = indexJ (value - leftsize) r
    | (value == leftsize) = indexJ (value - leftsize) r
    where 
        leftsize = getSize(size(tag l))

testList :: JoinList Size String
testList = (Append (Size 4)
                ( Append (Size 2) 
                    (Single (Size 1) "trick joke")
                    (Single (Size 1) "happy dude")
                )
                ( Append (Size 2) 
                    (Single (Size 1) "smile corn")
                    (Single (Size 1) "drown duck")
                )
           )

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ value s@(Single _ y)
    | (value == 0) = s
    | otherwise = Empty
dropJ value s@(Append m l r)
    | (value < leftsize) = (dropJ value l) +++ r
    | (value > leftsize) = dropJ (value - leftsize) r
    | (value == leftsize) = r
    where 
        leftsize = getSize(size(tag l))

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ value s@(Single _ y)
    | (value == 1) = s
    | otherwise = Empty
takeJ value s@(Append m l r)
    | (value < leftsize) = takeJ value l
    | (value > leftsize) = l +++ (takeJ (value - leftsize) r) 
    | (value == leftsize) = l
    where
        leftsize = getSize(size(tag l))

--3
scoreLine :: String -> JoinList Score String
scoreLine string = Single (scoreString string) string

scoreString :: String -> Score
scoreString string = foldl (\acc x -> acc + score(x)) 0 string
--4
instance Buffer (JoinList (Score, Size) String) where
  toString jl   = show jl
  fromString string = foldr (+++) Empty (map (\x -> Single (scoreString x, Size 1) x) (lines string))
  line n b     = indexJ n b
  replaceLine n l b = takeJ n b +++ (Single (scoreString l, Size 1) l) +++ dropJ (n + 1) b
  numLines b   = getSize(size(tag b))
  value b      = getScore(fst(tag b))

getScore :: Score -> Int
getScore (Score i) = i
















