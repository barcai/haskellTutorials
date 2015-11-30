-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 11 - due: 3/4 Dec.

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (x, _, _, _, _) = x
alph   (_, x, _, _, _) = x
start  (_, _, x, _, _) = x
final  (_, _, _, x, _) = x
trans  (_, _, _, _, x) = x


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta (_, _, _, _, []) _ _ = []
delta (a, b, c, d, ((src,chr,dest):transitions) ) source symbol
    | src == source && chr == symbol = dest : (delta (a, b, c, d, transitions) (source) (symbol))
    | otherwise = (delta (a, b, c, d, transitions) source symbol)


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs
    where
      acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
      acceptsFrom m q [] = q `elem` final m
      acceptsFrom m q (x:xs) = or [acceptsFrom m y xs | y <- (delta m q x)]


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort . nub


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta = undefined


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next = undefined


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable = undefined


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal = undefined


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans = undefined


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]


-- Optional Material
--11.
aut :: String -> FSM Int
aut str = undefined


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_aut1 n = accepts (aut n) n
prop_aut2 n m = (m' == n') || (not $ accepts (aut n') m')
                where m' = safeString m
                      n' = safeString n

-- 12.
complement :: (Ord q) => FSM q -> FSM q
complement m = undefined

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ aut n') m'
                      && (not $ accepts (complement $ aut n') n)
                      where n' = safeString n
                            m' = safeString m
-- 13.
unionFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
unionFSM a b = undefined
        
prop_union n m l =  accepts (unionFSM (aut n') (aut m')) l' == (accepts (aut n') l' || accepts (aut m') l') &&
                    accepts (unionFSM (aut n') (aut m')) n' && accepts (unionFSM (aut n') (aut m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l
-- 14.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM a b = undefined
                
prop_intersect n m l = accepts (intersectFSM (aut n') (aut m')) l' == (accepts (aut n') l' && accepts (aut m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

-- 15.
intFSM :: Ord q => FSM q -> FSM Int
intFSM a = undefined


prop_intFSM a b = intFSM (aut a') `accepts` b' == (aut a') `accepts` b' && 
                  (intFSM (complement (aut a')) `accepts` b' == (complement (aut a') `accepts` b'))
  where a' = safeString a
        b' = safeString b

-- 15.
concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM a b = undefined

prop_concat n m l = accepts (concatFSM (aut n') (aut m')) (n'++m') &&
                    accepts (concatFSM (aut n') (aut m')) l' == (l' == n'++m')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l
-- 16.
star :: (Ord q) => FSM q -> FSM q
star a = undefined
         
prop_star a n = (star $ aut a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ aut a') `accepts` ""
      where a' = safeString a


prop1 a b = star ((aut a') `unionFSM` (aut b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((aut a') `intersectFSM` (intFSM ((aut b') `unionFSM` (aut a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b
deterministic = undefined


