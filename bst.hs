module Bst where


data Bst a = Leaf
           | Node (Bst a) a (Bst a)
           deriving (Show)

insert :: (Ord a) => Bst a -> a -> Bst a
insert Leaf v = Node Leaf v Leaf
insert (Node l v r) new | v > new = Node (insert l new) v r
                        | otherwise = Node l v (insert r new)

contains :: (Ord a) => Bst a -> a -> Bool
contains Leaf _ = False
contains (Node l v r) t | v == t = True
                        | v > t = contains l t
                        | otherwise = contains r t
