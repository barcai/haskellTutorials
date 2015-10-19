-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers text = map (toUpper) text

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (\x -> fromIntegral x / 100) xs

-- d.
uppers' :: String -> String
uppers' text = [toUpper character | character <- text]

prop_uppers :: String -> Bool
prop_uppers text = uppers text == uppers' text



-- 2. Filter
-- a.
alphas :: String -> String
alphas text = filter isAlpha text

-- b.
rmChar ::  Char -> String -> String
rmChar char text = filter (\x -> x /= (toLower char) && x /= (toUpper char)) text

-- c.
above :: Int -> [Int] -> [Int]
above least numbers  = filter (\x -> x > least) numbers

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals pairs = filter (\(x,y) -> x /= y) pairs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp char text = [textChar | textChar <- text, textChar /= toLower char, textChar /= toUpper char]

prop_rmChar :: Char -> String -> Bool
prop_rmChar char text = rmChar char text == rmCharComp char text



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = undefined

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = undefined

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = undefined

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = undefined

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec = undefined

andFold :: [Bool] -> Bool
andFold = undefined

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec = undefined

concatFold :: [[a]] -> [a]
concatFold = undefined

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec = undefined

rmCharsFold :: String -> String -> String
rmCharsFold = undefined

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform = undefined

-- b.
valid :: Matrix -> Bool
valid = undefined

-- 6.

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- Optional material
-- 9.