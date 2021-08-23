--Ivan Malison
--Lab 4 - Math 121 - Jim Fix

module IAMLab4 where

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (l:ls) = (f l):myMap f ls

addOne :: Int->Int
addOne x = x+1

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ a [] = a
myFoldr f a  (l:ls) =f l (myFoldr f a ls)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f l
 |f (head l) == True = head l:myFilter f (tail l)
 |otherwise = myFilter f (tail l)

isEven :: Int->Bool
isEven x
 |x `mod` 2 == 0 = True
 |otherwise = False

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (a:as) (b:bs) = (f a b):(myZipWith f as bs)

--Exercise 4 Bonus:

--anySeq is a generalized version of binSeq.  Given an integer n and a list, it returns every
--possible sequence of length n made up of elements from the list.
anySeq :: Int->[Int]->[[Int]]
anySeq 0 _ = []
anySeq n l = anySeqCounter n (listoflists l) l

--(read anySeqh first) anySeqCounter's job is to run anySeqh n times so that the lists it produces
--are of length n.
anySeqCounter :: Int->[[Int]]->[Int]->[[Int]]
anySeqCounter n a l
 |n > 1 = anySeqCounter (n-1) (anySeqh a l) l
 |otherwise = a

--anySeqh and and anySeqh2 take a list of lists and a list and append every item in the list to
--each list in the list of lists.  Specifically, anySeqh takes the head of the list of
--lists and sends it to anySeqh2, and appends that to anySeqh of the tail of the list, ensuring that
--anySeqh2 is run on every list in the list of lists.
anySeqh :: [[Int]]->[Int]->[[Int]]
anySeqh [] _ = []
anySeqh (a:as) l = (anySeqh2 a l)++(anySeqh as l)

--anySeqh2 takes the head of the lists of lists in anySeqh and prepends every item in the list from
--anySeqh recursively.
anySeqh2 :: [Int]->[Int]->[[Int]]
anySeqh2 _ [] = []
anySeqh2 a (l:ls) = (l:a):anySeqh2 a ls

