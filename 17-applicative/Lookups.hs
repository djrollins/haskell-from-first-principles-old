module Lookups where

import Data.List (elemIndex)

xs = [1, 2, 3]
ys = [4, 5, 6]

added :: Maybe Integer
added = (+3) <$> (lookup 2 $ zip xs ys)

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> x <*> y

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> a <*> b

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y')
