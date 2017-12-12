{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.WordNum (numToWord, wordToNum, isNumWord, numWords) where

import Data.Bool (bool)
import Data.Char
import Data.List.Split.Internals
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T


ones :: Integral a => [(Text, a)]
ones = [("one",1), ("two",2), ("three",3), ("four",4), ("five",5), ("six",6), ("seven",7),
        ("eight",8), ("nine",9)]

teens :: Integral a => [(Text, a)]
teens = [("ten",10), ("eleven",11), ("twelve",12), ("thirteen",13), ("fourteen",14),
         ("fifteen",15), ("sixteen",16), ("seventeen",17), ("eighteen",18), ("nineteen",19)]

tens :: Integral a => [(Text, a)]
tens = [("ten",10), ("twenty",20), ("thirty",30), ("forty",40), ("fifty",50), ("sixty",60),
        ("seventy",70), ("eighty",80), ("ninety",90)]

-- Maps a group word to its power of 10 to calculate a group multiplier
groupList :: Integral a => [(Text, a)]
groupList = [("thousand",3), ("million",6), ("billion",9), ("trillion",12), ("quadrillion",15),
             ("quintillion",18), ("sextillion",21), ("septillion",24), ("octillion",27),
             ("nonillion",30), ("decillion",33), ("undecillion",36), ("duodecillion",39),
             ("tredecillion",42), ("quattuordecillion",45), ("quindecillion",48),
             ("sexdecillion",51), ("septendecillion",54), ("octodecillion",57),
             ("novemdecillion",60), ("vigintillion",63)]


numWords :: [Text]
numWords = map fst $ concat [ones, teens, tens, groupList]

isNumWord :: Text -> Bool
isNumWord word = all isNumToken (T.words word)
  where
    isNumToken :: Text -> Bool
    isNumToken "" = False
    isNumToken token
        | isDigitList  = True
        | otherwise    = mapMember token ones || mapMember token teens || mapMember token tens
                            || mapMember token groupList || token == "hundred"
        where
            isDigitList = T.all isDigit token
                            || ('-' == (T.head token) && T.all isDigit (T.tail token))

{--------------------------------------------------------------------------------------------------}
{---                                   NUMBER TO WORD CONVERSION                                ---}
{--------------------------------------------------------------------------------------------------}

-- Converts an Int or Integer to its Text representation
numToWord :: Integral a => a -> Text
numToWord n
    | n < 0      = T.append "negative " $ numToWord (-n)
    | n == 0     = "zero"
    | n >= 10^65 = error "numToWord: Doesn't support numbers bigger than vigintillions! (10^65-1)"
    | isHundred  = T.append (groupToWord d100) " hundred"
    | otherwise  = T.unwords $ reverse buildGroups
    where
        buildGroups = map (uncurry T.append) (filter (\(block,_) -> not $ T.null block) (zip wordGroups groups))
        wordGroups = map groupToWord $ splitNum n
        groups = "" : map (\(group,_) -> T.append " " group) groupList

        isHundred = n <= 9000 && r1000 /= 0 && r100 == 0
        r1000 = n `rem` 1000
        (d100,r100) = n `quotRem` 100

-- Splits a number into groups in reverse order
splitNum :: Integral a => a -> [a]
splitNum n
    | d == 0  = [n]
    | otherwise = m : splitNum d
    where
        (d,m) = n `quotRem` 1000

-- Converts a number to a word given a number translation map
toWord :: Integral a => a -> [(Text, a)] -> Text
toWord n table = fromMaybe T.empty (lookup n (map swap table))
  where
    swap :: Integral a => (Text, a) -> (a, Text)
    swap (x,y) = (y,x)

-- Converts a group of digits to a number.
-- A group is usually 1 to 3 digits, but also includes numbers that are multiples of 100, but not
-- 1000, such as 1900. Words are not comma-separated as per the Chicago Manual of Style. This
-- prevents numbers from potentially seeming like a list of smaller numbers.
groupToWord :: Integral a => a -> Text
groupToWord n = T.unwords $ numWords n
  where
    numWords :: Integral a => a -> [Text]
    numWords r
        | r < 0                = "negative" : numWords (-r)
        | r == 0               = []
        | r < 10               = [toWord r ones]
        | r < 20               = [toWord r teens]
        | r < 100 && r10 /= 0  = [T.concat [toWord n10 tens, "-", toWord r10 ones]]
        | r < 100              = toWord n10 tens : numWords r10
        | r < 1000             = toWord d100 ones : "hundred" : numWords r100
        | otherwise            = error "groupToWord: not a 3-digit or hundred group"
        where
            (n10, r10) = (r - r10, r `rem` 10)
            (d100, r100) = r `quotRem` 100


{--------------------------------------------------------------------------------------------------}
{---                                   WORD TO NUMBER CONVERSION                                ---}
{--------------------------------------------------------------------------------------------------}

-- Converts a single block to an Int or Integer
wordToNum :: (Integral a, Read a) => Text -> a
wordToNum s
    | T.null s    = 0
    | isNegative  = (-1) * wordToNum (T.unwords $ tail (T.words $ T.toLower s))
    | isDigitStr  = read $ T.unpack cleanedNum
    | otherwise   = fromGroups $ splitGroups $ T.toLower s
    where
        isNegative = head (T.words s) `elem` ["negative", "minus"]
        isDigitStr = T.all isDigit cleanedNum
                    || (T.head s == '-' && T.all isDigit (T.tail cleanedNum))
        cleanedNum = T.replace ", " T.empty s -- digit string without spaces or commas

-- Splits a full number into blocks on group words, keeping delimeters at the end of each group.
splitGroups :: Text -> [Text]
splitGroups str = map T.unwords $ split (dropBlanks $ oneOf groupWords) (T.words str)
  where
    groupWords = map fst groupList

-- Calculates the total sum of all groups' values
fromGroups :: Integral a => [Text] -> a
fromGroups (block:group:bs) = fromBlock (splitBlock block) * multiplier group + fromGroups bs
fromGroups [block] = fromBlock (splitBlock block)
fromGroups [] = 0

-- Calculates a group's multiplier, or defaults to 1 (10^0) if the group isn't valid
multiplier :: Integral a => Text -> a
multiplier g = 10 ^ fromMaybe 0 (lookup g groupList)

-- Splits a block on "hundred", keeping the delimeter, and cleaning spacing and punctuation
splitBlock :: Text -> [Text]
splitBlock block = map clean $ T.splitOn "hundred" block
  where
    -- Switches all hyphens to spaces and removes all other non-letters
    clean :: Text -> Text
    clean w = (T.unwords . T.words) $ T.filter stripPunctuation $ T.map hyphenToSpace w

    stripPunctuation :: Char -> Bool
    stripPunctuation c = c == ' ' || isLetter c

    hyphenToSpace :: Char -> Char
    hyphenToSpace c = bool ' ' c (c /= '-')

-- Calculates a block's value, defaulting to 0 if there's invalid input
-- Words like "one eighty" will be treated as 180, and "nineteen ninety" will be 1990.
fromBlock :: Integral a => [Text] -> a
fromBlock [x,"hundred",y] = fromBlock (T.words x) * 100 + fromBlock (T.words y)
fromBlock [x,"hundred"] = fromBlock (T.words x) * 100
fromBlock [x,y]
    | mapMember x (tail tens) && mapMember y ones  = fromWord x tens + fromWord y ones
    | otherwise                                    = fromBlock [x] * 100 + fromBlock [y]
fromBlock [w]
    | length (T.words w) > 1  = fromBlock (T.words w)
    | mapMember w ones        = fromWord w ones
    | mapMember w teens       = fromWord w teens
    | mapMember w tens        = fromWord w tens
    | otherwise               = 0
fromBlock (x:ys) = fromBlock [x] * 100 + fromBlock ys
fromBlock _ = 0

-- Converts a word to its numeric value from a given conversion table
fromWord :: Integral a => Text -> [(Text, a)] -> a
fromWord word table = fromMaybe 0 (lookup word table)

-- Determines whether a word is a member of a conversion table
mapMember :: Integral a => Text -> [(Text, a)] -> Bool
mapMember w table = isJust (lookup w table)
