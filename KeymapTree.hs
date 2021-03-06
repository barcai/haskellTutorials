-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Show)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = max (1 + depth left) (1 + depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v left right) = toList left ++ [(k,v)] ++ toList right
 
-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
      f Leaf = Nothing
      f (Node k v left right) | key == k  = Just v
                              | key <= k  = get key left
                              | otherwise = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,a): list) = set k a (fromList list)


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k v left right)
  | key == k    = Node k v (filterLT key left) Leaf
  | key <= k    = filterLT key left
  | otherwise   = Node k v (filterLT key left) (filterLT key right)

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k v left right)
  | key == k    = Node k v Leaf (filterGT key right)
  | key >= k    = filterGT key right
  | otherwise   = Node k v (filterGT key left) (filterGT key right)

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge tree1 tree2 = fromList $ (toList tree1) ++ (toList tree2)

-- Quickcheck does not work
-- prop_merge :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
-- prop_merge tree1 tree2 = (toList (merge tree1 tree2)) == sort . nub ((toList tree1) ++ (toList tree2))

-- Exercise 14

del :: (Ord k, Eq a) => k -> Keymap k a -> Keymap k a
del _ Leaf = Leaf
del key (Node k v left right)
  | (get key (Node k v left right)) == Nothing = (Node k v left right)
  | key == k                                   = merge left right
  | otherwise                                  = (Node k v (del key left) (del key right))

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select f (Node k v left right) 
  | f v       = (Node k v (select f left) (select f right))
  | otherwise = merge (select f left) (select f right)
