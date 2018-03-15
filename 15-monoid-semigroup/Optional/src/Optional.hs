module Optional where

data Optional a = Nada | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (a `mappend` b)
  mappend x        Nada     = x
  mappend Nada     x        = x

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Show, Eq)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) f = f
  mappend f             _ = f
