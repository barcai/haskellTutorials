-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 15/16 Oct.

import Data.Char
import Data.List
import Data.List.Split
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
encipherStr n text = map (encipher n) (normalize text)

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(char2, char1) | (char1, char2) <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((char1, char2):key) = (char2, char1) : reverseKeyRec key

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key

-- 9.
decipher :: Int -> Char -> Char
decipher n character = lookUpRec character (reverseKeyRec (makeKey n))

decipherStr :: Int -> String -> String
decipherStr _ [] = []
decipherStr n (char:text)
          | char `elem` ['A'..'Z'] = decipher n char : decipherStr n text
          | char `elem` ['0'..'9'] = char : decipherStr n text
          | char == ' ' = char : decipherStr n text
          | otherwise = decipherStr n text

-- 10.
contains :: String -> String -> Bool
contains [] [] = True
contains [] _ = False
contains word subset = not (null [x | x <- [0..(length word)], isPrefixOf subset (drop x word)])

-- 11.
candidates :: String -> [(Int, String)]
candidates text = [(x, decipherStr x text) | x <- [1..26], check (decipherStr x text)]
                  where check string = contains string "THE" || contains string "AND"



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive text | length (take 5 text) == 5 = take 5 text : splitEachFive (drop 5 text)
                   | otherwise = (text ++ replicate (5 - length (take 5 text)) 'X') : splitEachFive (drop 5 text)


-- 13.
prop_transpose :: String -> Bool
prop_transpose text = splitEachFive text == transpose (transpose (splitEachFive text))

-- 14.
--helper
putBackTogether :: [String] -> String
putBackTogether [] = []
putBackTogether (x:xs) = x ++ putBackTogether xs
--main function
encrypt :: Int -> String -> String
encrypt n text = putBackTogether $ (transpose (splitEachFive (encipherStr n text)))

-- 15.
decrypt :: Int -> String -> String
decrypt n text = decipherStr n (putBackTogether (transpose (chunksOf ((length text) `div` 5) text)))

-- Challenge (Optional)

-- 16.
helper :: String -> [(Char, Int)]
helper text = rmDup [(char,0) | char <- text]
             where
              rmDup [] = []
              rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)

countFreqs :: String -> [(Char, Int)]
countFreqs [] = []
countFreqs (char:text) = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined