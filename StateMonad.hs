import           Control.Monad.State
import           System.Random
-- instance Monad (State s) where
--     return x = State $ \s -> (x,s)
--     (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                         (State g) = f a
--                                     in  g newState

-- Example for stack
type Stack = [Int]

pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

-- Wrap random number generating as state monad
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt

  return (a,b,c)

