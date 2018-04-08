{-# LANGUAGE InstanceSigs #-}
module ChapterExercises where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f g = State $ \s ->
    let (a, s') = runState g s
    in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  f <*> g = State $ \s ->
      let (fab, s') = runState f s
          (a, s'') = runState g s'
      in (fab a, s'')

instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  f >>= g = State $ \s ->
    let (a, s') = runState f s
    in runState (g a) s'

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec st = snd . runState st

eval :: State s a -> s -> a
eval st = fst . runState st

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
