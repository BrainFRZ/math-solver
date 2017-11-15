module MathSolver.NLP.WordNum (numToWord) where

import Data.Char
import Data.List
import Data.Maybe


ones :: Integral a => [(a, String)]
ones = [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"), (6,"six"), (7,"seven"),
        (8,"eight"), (9,"nine")]

teens :: Integral a => [(a, String)]
teens = [(10,"ten"), (11,"eleven"), (12,"twelve"), (13,"thirteen"), (14,"fourteen"),
         (15,"fifteen"), (16,"sixteen"), (17,"seventeen"), (18,"eighteen"), (19,"nineteen")]

tens :: Integral a => [(a, String)]
tens = [(10,"ten"), (20,"twenty"), (30,"thirty"), (40,"forty"), (50,"fifty"), (60,"sixty"),
        (70,"seventy"), (80,"eighty"), (90,"ninety")]

groups :: [String]
groups = ["", " thousand", " million", " billion", " trillion"]


toWord :: Integral a => a -> [(a, String)] -> String
toWord n table = fromJust (lookup n table)


numToWord :: Integral a => a -> String
numToWord n
    | n < 0      = "negative " ++ numToWord (-n)
    | n == 0     = "zero"
    | n >= 10^15 = error "Doesn't support numbers bigger than trillions"
    | isHundred  = groupToWord d100 ++ " hundred"
    | otherwise  = unwords $ reverse buildGroups
    where
        buildGroups = map join (filter (\(n,g) -> n /= "") (zip wordGroups groups))
        wordGroups = map groupToWord $ splitNum n
        join (w,g) = w ++ g

        isHundred = n <= 9000 && r1000 /= 0 && r100 == 0
        r1000 = n `rem` 1000
        (d100,r100) = n `quotRem` 100

groupToWord :: Integral a => a -> String
groupToWord n
    | n < 0     = "negative " ++ groupToWord (-n)
    | n == 0    = ""
    | otherwise = unwords $ numWords n

numWords :: Integral a => a -> [String]
numWords r
    | r <= 0               = []
    | r < 10               = [toWord r ones]
    | r < 20               = [toWord r teens]
    | r < 100 && r10 /= 0  = [toWord n10 tens ++ "-" ++ toWord r10 ones]
    | r < 100              = toWord n10 tens : numWords r10
    | r < 1000             = toWord d100 ones : "hundred" : numWords r100
    | otherwise            = error "groupToWord: not a 3-digit group"
    where
        (n10, r10) = (r - r10, r `rem` 10)
        (d100, r100) = r `quotRem` 100

-- Splits a number into groups in reverse order
splitNum :: Integral a => a -> [a]
splitNum n
    | d == 0  = [n]
    | otherwise = m : splitNum d
    where
        (d,m) = n `quotRem` 1000
