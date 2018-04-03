{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

import Control.Monad (join)

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader f) <*> (Reader g) = Reader $ (f <*> g)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName,
    dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = Dog <$> Reader dogName <*> Reader address
