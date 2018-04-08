module ThrowDown where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- :t randomR (1, 6) :: StdGen -> (Int, StdGen)
-- :t state :: (Monad m) => (s -> (a, s)) -> StateT s m a
-- :t state (randomR (1, 6)) :: Monad m => State StdGen m Int
-- fmap intToDie over State StdGen applies intToDie to the final Int type-argument
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

-- doesn't work because it just runs rollDie' with the same state over and over.
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN l g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, rolls) gen
      | sum >= l = (count, rolls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (succ count, intToDie die : rolls) nextGen

rollsToGet20 :: StdGen -> Int
rollsToGet20 = fst . rollsToGetN 20
