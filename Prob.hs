import           Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x,p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1%1)]
  Prob probFn <*> Prob xs = Prob $ concat $ map fn probFn
    where fn (f, p) = map (\(x,r) -> (f x, p * r)) xs

instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flatten (fmap f m)
