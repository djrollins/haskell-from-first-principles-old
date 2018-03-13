module OptionalMonoid where

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (x `mappend` y)
  mappend x        Nada     = x
  mappend Nada     x        = x

