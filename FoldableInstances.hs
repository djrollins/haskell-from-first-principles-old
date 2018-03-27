module FoldableInstances where

data Constant a b = Constant b
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four' a b = Four' a b b b

instance Foldable (Constant a) where
  foldr f y (Constant x) = f x y

instance Foldable (Two a) where
  foldr f y (Two _ x) = f x y

instance Foldable (Three a b) where
  foldr f y (Three _ _ x) = f x y

--instance Foldable (Three' a) where
--  foldr f y (Three' _ x1 x2) = f x1 (f x2 y)

instance Foldable (Three' a) where
  foldMap f (Three' _ x1 x2) = f x1 `mappend` f x2

instance Foldable (Four' a) where
  foldMap f (Four' _ x1 x2 x3) = f x1 `mappend` f x2 `mappend` f x3

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
