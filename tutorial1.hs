-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (8/9 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, even x]


-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = (x `div` 2):(halveEvensRec xs)
                     | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs) | x >= lo && x <= hi = x:(inRangeRec lo hi xs)
                        | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRange lo hi xs) == (inRangeRec lo hi xs)



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = sum [1 | x <- xs, x > 0]
-- d) Because list comprehensions only return lists and they cannot return other values than that


-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + (countPositivesRec xs)
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round ((fromIntegral x) * 0.90)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [x | x <- xs, x <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | x <= 19900 = x + pennypincherRec xs
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits stringstuff = product [digitToInt x | x <- stringstuff, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = (digitToInt x) * multDigitsRec xs
                     | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise xs = (toUpper (head xs)) : [toLower y | y <- tail xs]

-- Recursive version
-- Helper for the Recursive version
getStringToLower :: String -> String
getStringToLower []     = []
getStringToLower (x:xs) = toLower x : getStringToLower xs

capitaliseRec :: String -> String
capitaliseRec []     = []
capitaliseRec (x:xs) = toUpper x : getStringToLower xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title xs = capitalise (head xs) : [if length string >= 4 then capitalise string else getStringToLower string | string <- tail xs]

-- Recursive version
-- helper function to capitalize the first word if it was not done previously
titleRecHelper :: [String] -> [String]
titleRecHelper [] = []
titleRecHelper (x:xs) | length x >= 4 = capitaliseRec x    : titleRecHelper xs
                | otherwise     = getStringToLower x : titleRecHelper xs

titleRec :: [String] -> [String]
titleRec [] = []
titleRec [x] = [capitaliseRec x]
titleRec (x:xs) = capitaliseRec x : titleRecHelper xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind character position len wordList = [x | x <- wordList,
                                                (position >= 0) &&
                                                (length x > position) &&
                                                (x !! position == character) && 
                                                (length x == len)]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec character position len (x:wordList) 
    | position < 0 || length x <= position || len < position = []
    | (length x == len) && (x !! position == character) = x : crosswordFindRec character position len wordList
    | otherwise = crosswordFindRec character position len wordList

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind u d v a = crosswordFind u d v a == crosswordFindRec u d v a 



-- 9. search

-- List-comprehension version
search :: String -> Char -> [Int]
search word character = [y | (x,y) <- (zip word [0..]), x  == character] 


-- Helper for recursive
searchRec' :: [(Char, Int)] -> Char -> [Int]
searchRec' [] _ = []
searchRec' ((x,y):xs) character | x  == character = y : searchRec' xs character
                                | otherwise = searchRec' xs character
-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec word character = searchRec' (zip word [0..]) character

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search word character = search word character == searchRec word character


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains [] [] = True
contains [] _ = False
contains word subset = not (null [x | x <- [0..(length word)], isPrefixOf subset (drop x word)])

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] [] = True
containsRec [] _  = False
containsRec word subset 
        | isPrefixOf subset word = True
        | otherwise = containsRec (drop 1 word) subset

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains word subset = contains word subset == containsRec word subset
