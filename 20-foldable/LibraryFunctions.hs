module LibraryFunctions where

import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (==a))

foldableHead :: Foldable t => t a -> a
foldableHead = head . toList

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t
  | length t == 0 = Nothing
  | otherwise = Just $ foldr (min) (foldableHead t) t

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' t
  | length t == 0 = Nothing
  | otherwise = Just $ foldr (max) (foldableHead t) t

null' :: Foldable t => t a -> Bool
null' = (==0) . length . toList

length' :: Foldable t => t a -> Int
length' = rec . toList
  where rec [] = 0
        rec (x:xs) = 1 + length xs

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
