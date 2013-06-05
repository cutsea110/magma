module Data.Magma 
       ( Magma(..)
       , leaf
       , node
       , left
       , right
       , foldMap
       )where

class Magma a where
  (<>) :: a -> a -> a

data BinaryTree a = Leaf a 
                  | Node { left :: BinaryTree a 
                         , right :: BinaryTree a
                         }
                  deriving (Show, Read, Eq)

leaf :: a -> BinaryTree a
leaf = Leaf
node :: BinaryTree a -> BinaryTree a -> BinaryTree a
node = Node

foldMap :: Magma m => (a -> m) -> BinaryTree a -> m
foldMap f (Leaf x) = f x
foldMap f (Node l r) = foldMap f l <> foldMap f r
