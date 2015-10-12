-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 15/16 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs | n < 0 = error "Your number is smaller than 0"
            | n > length xs = error "Your number is too large"
            | otherwise = drop n xs ++ take n xs
-- Example: rotate 5 " catsI love" -> "I love cats"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp character keys = case [y| (x,y) <- keys, x == toUpper character] of
	                         [y] -> y
	                         [] -> character

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec character ((x,y):xs) | toUpper character == x = y
                           | otherwise = lookUpRec character xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp character keys = lookUp character keys == lookUpRec character keys

-- 5.
encipher :: Int -> Char -> Char
encipher n character = lookUpRec character (makeKey n)

-- 6.
normalize :: String -> String
normalize text = filter (isAlphaNum) $ map (toUpper) text

-- 7.
encipherStr :: Int -> String -> String
encipherStr = undefined

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey = undefined

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = undefined

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined
-- 9.
decipher :: Int -> Char -> Char
decipher = undefined

decipherStr :: Int -> String -> String
decipherStr = undefined

-- 10.
contains :: String -> String -> Bool
contains = undefined

-- 11.
candidates :: String -> [(Int, String)]
candidates = undefined



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive = undefined

-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 14.
encrypt :: Int -> String -> String
encrypt = undefined

-- 15.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined