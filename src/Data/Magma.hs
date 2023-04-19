{-# LANGUAGE DeriveFunctor, Rank2Types #-}
module Data.Magma 
       ( Magma(..)
       , BinaryTree(..)
       , cataBinaryTree
       , anaBinaryTree
       , foldMap
       , _Leaf
       , _Node
       , nodeLeft
       , nodeRight
       ) where

import Prelude hiding (foldMap, (<>))
import qualified Data.Foldable as F
import qualified Data.Monoid as M hiding ((<>))
import Data.Profunctor
import qualified Data.Semigroup as S hiding ((<>))
import Control.DeepSeq
import Control.Applicative
import Data.Traversable

class Magma a where
  (<>) :: a -> a -> a
  

instance Magma () where
  _ <> _ = ()

instance (Magma a, Magma b) => Magma (a, b) where
  (a, b) <> (a', b') = (a <> a', b <> b')

instance Magma a => Magma (M.Dual a) where
  M.Dual a <> M.Dual b = M.Dual (b <> a)

instance Magma (M.Endo a) where
  M.Endo f <> M.Endo g = M.Endo (f . g)

instance Magma M.All where
  M.All a <> M.All b = M.All (a && b)

instance Magma M.Any where
  M.Any a <> M.Any b = M.Any (a || b)

instance Num a => Magma (M.Sum a) where
  M.Sum a <> M.Sum b = M.Sum (a + b)

instance Num a => Magma (M.Product a) where
  M.Product a <> M.Product b = M.Product (a * b)

instance Magma (M.First a) where
  r@(M.First (Just _)) <> _ = r
  M.First Nothing <> r = r

instance Magma (M.Last a) where
  _ <> r@(M.Last (Just _)) = r
  r <> M.Last Nothing = r

instance Ord a => Magma (S.Min a) where
  S.Min a <> S.Min b = S.Min (min a b)

instance Ord a => Magma (S.Max a) where
  S.Max a <> S.Max b = S.Max (max a b)

instance M.Monoid m => Magma (S.WrappedMonoid m) where
  S.WrapMonoid a <> S.WrapMonoid b = S.WrapMonoid (M.mappend a b)

data BinaryTree a = Leaf a 
                  | Node (BinaryTree a) (BinaryTree a)
                  deriving (Show, Read, Eq, Ord, Functor)

cataBinaryTree :: (a -> r) -> (r -> r -> r) -> BinaryTree a -> r
cataBinaryTree f _ (Leaf a) = f a
cataBinaryTree f g (Node l r) = g (cataBinaryTree f g l) (cataBinaryTree f g r)

anaBinaryTree :: (b -> Either a (b, b)) -> b -> BinaryTree a
anaBinaryTree f = go where
  go b = case f b of
    Left a -> Leaf a
    Right (c, d) -> Node (go c) (go d)

foldMap :: Magma m => (a -> m) -> BinaryTree a -> m
foldMap f (Leaf x) = f x
foldMap f (Node l r) = foldMap f l <> foldMap f r

instance F.Foldable BinaryTree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = F.foldMap f l `M.mappend` F.foldMap f r

instance Magma (BinaryTree a) where
  (<>) = Node

instance Traversable BinaryTree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  Leaf f <*> Leaf x = Leaf (f x)
  Leaf f <*> Node l r = Node (f <$> l) (f <$> r)
  Node l r <*> t = Node (l <*> t) (r <*> t)

instance Monad BinaryTree where
  Leaf a >>= k = k a
  Node l r >>= k = Node (l >>= k) (r >>= k)

instance NFData a => NFData (BinaryTree a) where
  rnf (Leaf a) = rnf a
  rnf (Node l r) = rnf l `seq` rnf r

-- | @'_Leaf' :: Prism' ('BinaryTree' a) a@
_Leaf :: forall p f a. (Choice p, Applicative f) => p a (f a) -> p (BinaryTree a) (f (BinaryTree a))
_Leaf = dimap go (either pure (fmap Leaf)) . right' where
  go (Leaf a) = Right a
  go t = Left t

-- | @'_Node' :: Prism' ('BinaryTree' a) ('BinaryTree' a, 'BinaryTree' a)@
_Node :: forall p f a. (Choice p, Applicative f) => p (BinaryTree a, BinaryTree a) (f (BinaryTree a, BinaryTree a)) -> p (BinaryTree a) (f (BinaryTree a))
_Node = dimap go (either pure (fmap (uncurry Node))) . right' where
  go (Node l r) = Right (l, r)
  go t = Left t

-- | @'nodeLeft' :: Traversal' ('BinaryTree' a) ('BinaryTree' a)@
nodeLeft :: Applicative f => (BinaryTree a -> f (BinaryTree a)) -> BinaryTree a -> f (BinaryTree a)
nodeLeft f (Node l r) = (\l' -> Node l' r) <$> f l
nodeLeft _ t = pure t

-- | @'nodeRight' :: Traversal' ('BinaryTree' a) ('BinaryTree' a)@
nodeRight :: Applicative f => (BinaryTree a -> f (BinaryTree a)) -> BinaryTree a -> f (BinaryTree a)
nodeRight f (Node l r) = (\r' -> Node l r') <$> f r
nodeRight _ t = pure t
