module Dice where
import State
import System.Random
-- class Random a where
-- randomR :: RandomGen g => (a, a) -> g -> (a, g)
-- randomR (lo, hi) gen - returns value x st lo<=x<=hi and a new generator
-- instance RandomGen StdGen
-- mkStdGen :: Int -> StdGen
-- mkStdGen returns a StdGen with a given seed

type GenState = State StdGen

rollD6 :: GenState Int
rollD6 = do generator <- get
            let( value, newGenerator ) = randomR (1,6) generator
            put newGenerator
            return value

rollNd6 :: Int -> GenState [Int]
rollNd6 0 = return []
rollNd6 n = do
  x <- rollD6
  xs <- rollNd6 (n-1)
  return (x:xs)
