module MathSolver.NLP.WordNum (numToWord) where

import Data.Char
import Data.List
import Data.Maybe


ones :: Integral a => [(String, a)]
ones = [("one",1), ("two",2), ("three",3), ("four",4), ("five",5), ("six",6), ("seven",7),
        ("eight",8), ("nine",9)]

teens :: Integral a => [(String, a)]
teens = [("ten",10), ("eleven",11), ("twelve",12), ("thirteen",13), ("fourteen",14),
         ("fifteen",15), ("sixteen",16), ("seventeen",17), ("eighteen",18), ("nineteen",19)]

tens :: Integral a => [(String, a)]
tens = [("ten",10), ("twenty",20), ("thirty",30), ("forty",40), ("fifty",50), ("sixty",60),
        ("seventy",70), ("eighty",80), ("ninety",90)]

groups :: [String]
groups = ["", " thousand", " million", " billion", " trillion", " quadrillion", " quintillion",
          " sextillion", " septillion", " octillion", " nonillion", " decillion", " undecillion",
          " duodecillion", " tredecillion", " quattuordecillion", " quindecillion", " sexdecillion",
          " septendecillion", " octodecillion", " novemdecillion", " vigintillion"]

-- Maps a word to its power of 10
groupList :: Integral a => [(String, a)]
groupList = [("thousand",3), ("million",6), ("billion",9), ("trillion",12), ("quadrillion",15),
             ("quintillion",18), ("sextillion",21), ("septillion",24), ("octillion",27),
             ("nonillion",30), ("decillion",33), ("undecillion",36), ("duodecillion",39),
             ("tredecillion",42), ("quattuordecillion",45), ("quindecillion",48),
             ("sexdecillion",51), ("septendecillion",54), ("octodecillion",57),
             ("novemdecillion",60), ("vigintillion",63)]


{--------------------------------------------------------------------------------------------------}
{---                                   NUMBER TO WORD CONVERSION                                ---}
{--------------------------------------------------------------------------------------------------}

-- Converts an Int or Integer to its String representation
numToWord :: Integral a => a -> String
numToWord n
    | n < 0      = "negative " ++ numToWord (-n)
    | n == 0     = "zero"
    | n >= 10^65 = error "Doesn't support numbers bigger than vigintillions! (10^65-1)"
    | isHundred  = groupToWord d100 ++ " hundred"
    | otherwise  = unwords $ reverse buildGroups
    where
        buildGroups = map (uncurry (++)) (filter (\(n,g) -> n /= "") (zip wordGroups groups))
        wordGroups = map groupToWord $ splitNum n

        isHundred = n <= 9000 && r1000 /= 0 && r100 == 0
        r1000 = n `rem` 1000
        (d100,r100) = n `quotRem` 100

-- Converts a number to a word given a number translation map
toWord :: Integral a => a -> [(String, a)] -> String
toWord n table = fromMaybe "" (lookup n (map swap table))
  where
    swap :: Integral a => (String, a) -> (a, String)
    swap (x,y) = (y,x)

-- Converts a group of digits to a number.
-- A group is usually 1 to 3 digits, but also includes numbers that are multiples of 100, but not
-- 1000, such as 1900. Words are not comma-separated as per the Chicago Manual of Style. This
-- prevents numbers from potentially seeming like a list of smaller numbers.
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
    | otherwise            = error "groupToWord: not a 3-digit or hundred group"
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


{--------------------------------------------------------------------------------------------------}
{---                                   WORD TO NUMBER CONVERSION                                ---}
{-------------------------------------------------------------------------------------------------}

-- Converts a word to its numeric value from a given conversion table
fromWord :: Integral a => String -> [(String, a)] -> a
fromWord word table = fromMaybe 0 (lookup word table)
