module Bst where

import Control.Monad (join)


data Bst a = Leaf
           | Node (Bst a) a (Bst a)
           deriving (Show)

insert :: (Ord a) => Bst a -> a -> Bst a
insert Leaf x = Node Leaf x Leaf
insert (Node l v r) x 
  | v > x = Node (insert l x) v r
  | otherwise = Node l v (insert r x)

contains :: (Ord a) => Bst a -> a -> Bool
contains Leaf _ = False
contains (Node l v r) x
  | v > x = contains l x
  | v < x = contains r x
  | otherwise = True

del :: (Ord a) => Bst a -> a -> Bst a
del Leaf _ = Leaf
del (Node l v r) x
  | v > x = Node (del l x) v r
  | v < x = Node l v (del r x)
  | otherwise = merge l r

merge :: (Ord a) => Bst a -> Bst a -> Bst a
merge Leaf Leaf = Leaf
merge Leaf n = n
merge n Leaf = n
merge n1@(Node l1 v1 r1) n2@(Node l2 v2 r2)
  | v1 > v2 = Node (merge n2 l1) v1 r1
  | otherwise = Node (merge n1 l2) v2 r2

getVal :: (Ord a) => Bst a -> a
getVal (Node l v r) = v

toList :: (Ord a) => Bst a -> [a]
toList Leaf = []
toList (Node l v r) = toList l ++  [v] ++ toList r 

middleEl :: (Ord a) => [a] -> a
middleEl l = head rh
  where
    (_, rh) = listToHalves l

listToHalves :: (Ord a) => [a] -> ([a], [a])
listToHalves l = splitAt (length l `div` 2) l

listToThree :: (Ord a) => [a] -> ([a], a, [a])
listToThree l = (lh, middleEl l, drop 1 rh)
  where
    (lh, rh) = listToHalves l

balanceHelp :: (Ord a) => [a] -> Bst a
balanceHelp [] = Leaf
balanceHelp l = Node (balanceHelp lh) m (balanceHelp rh)
  where
    (lh, m, rh) = listToThree l

balance :: (Ord a) => Bst a -> Bst a
balance Leaf = Leaf
balance bst = balanceHelp l
  where l = toList bst

------------
-- Sample stuff 
sRoot = Node Leaf 10 Leaf
one = insert sRoot 5
two = insert one 15
three = insert two 20
four = insert three 2
five = insert four 11
six = insert five 8
sampleTree = insert six 13

unbalanced = (Node Leaf 1 (Node Leaf 2 (Node Leaf 3 (Node Leaf 4 (Node Leaf 5 (Node Leaf 7 Leaf))))))
