{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

-- Rearrabge types to work (Woops, I rearranged the Functor instance!)
data Sum a b = First a | Second b

instance Functor (Sum e) where
  fmap _ (First a)  = First a
  fmap f (Second a) = Second $ f a

data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = R a b a | L b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (R a b c) = R   a  (f b)  c
  fmap f (L a b c) = L (f a)  b  (f c)

-- Write the Functor instances

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut (fmap g f)

data Prappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Prappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f (fmap h g)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious g1 g2 g3) = Notorious g1 g2 (fmap f g3)

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read r)    = Read (f . r)
