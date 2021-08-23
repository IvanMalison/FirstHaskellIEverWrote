--Ivan Malison
--Lab 1 - Computer Science 121 - Jim Fix

module IAMLab1 where

ctof :: Int->Int
ctof z = (z * 9 `div` 5) + 32

reverse2digits :: Int->Int
reverse2digits g = g `div` 10 + (g `mod` 10) * 10

changetotal :: Int -> Int -> Int -> Int -> Int
changetotal l m n o = l*25 + m*10 + n*5 + o

multipleof10 :: Int->Bool
multipleof10 x = x `mod` 10 == 0

QuaSolns :: Int->Int
QuaSolns a b c
 |b^2 - 4*a*c > 0 = 2
 |b^2 - 4*a*c == 0 = 1
 |b^2 - 4*a*C < 0 = 0
