--Ivan Malison
--Math 121 - Jim Fix - Lab 2

module IAMLab2 where

count :: Int->[Int]->Int
count x [] = 0 
count x (y:ys)
 |y - x == 0 = 1 + count x ys
 |otherwise = count x ys

lastOf :: [Int]->Int 
lastOf (a:as)
 |as == [] = a
 |otherwise = lastOf as

insertAt :: Int->Int->[Int]->[Int]
insertAt n x [] = [x]
insertAt n x (l:ls)
 |n > 1 = l:insertAt (n-1) x ls
 |n == 1 = x:l:ls

