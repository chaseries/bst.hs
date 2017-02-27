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

delete :: (Ord a) => Bst a -> a -> Bst a
delete (Node l v r) x
  | v > x = Node (delete l x) v r
  | v < x = Node l v (delete r x)
  | otherwise = Leaf

toList :: (Ord a) => Bst a -> [a]
toList Leaf = []
toList (Node l v r) = toList l ++  [v] ++ toList r 

middleEl :: (Ord a) => [a] -> a
middleEl l = head rh
  where
    (_, rh) = splitAt (length l `div` 2) l

balance :: (Ord a) => Bst a -> Bst a
balance Leaf = Leaf
balance bst@(Node l v r) =



------------
-- Sample stuff 
sRoot = Node Leaf 10 Leaf
one = insert sRoot 5
two = insert one 15
three = insert two 20
four = insert three 2
five = insert four 11
six = insert five 8
