-- Informatics 1 Functional Programming
-- December 2012
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: Int -> [Int] -> [Int]
f rep list = [if even y then 0 else x | (x,y) <- zip list [0..]]

-- 1b

g :: Int -> [Int] -> [Int]
g _ [] = []
g rep [x] = [rep]
g rep (x:y:xs) = rep : y : (g rep xs)

-- Question 2

-- 2a

p :: [Int] -> Bool
p list = and [if (x `elem` [10..100] && odd x) then False else True | x <- list]

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs)
    | x >= 10 && x <= 100 && odd x = False
    | otherwise = q xs

-- 2c

r :: [Int] -> Bool
r list = foldr (&&) True (map even (filter (`elem` [10..100]) list))

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X          =  "X"
showProp F          =  "F"
showProp T          =  "T"
showProp (Not p)    =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)  =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval X bool = bool
eval T _ = True
eval F _ = False
eval (Not p) bool = not (eval p bool)
eval (p :|: q) bool = (eval p bool) || (eval q bool)

-- 3b

simplify :: Prop -> Prop
simplify (Not T) = F
simplify (Not F) = T
simplify (Not (Not p)) = simplify p
simplify (T :|: p) = T
simplify (F :|: p) = simplify p
simplify (p :|: T) = T
simplify (p :|: F) = simplify p
simplify (p :|: q) = if p == q then simplify p else simplify p :|: simplify q
simplify p = p