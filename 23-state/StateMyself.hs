{-# LANGUAGE InstanceSigs #-}

module StateMyself where

-- "L'Etat, c'est moi." means "I am the state"
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f g = Moi $ \s ->
    let (a, s') = runMoi g s
    in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  f <*> g = Moi $ \s ->
      let (fab, s') = runMoi f s
          (a, s'') = runMoi g s'
      in (fab a, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  f >>= g = Moi $ \s ->
    let (a, s') = runMoi f s
    in runMoi (g a) s'
