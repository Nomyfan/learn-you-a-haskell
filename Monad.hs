applyMaybe :: Maybe a -> (a -> Maybe b)  -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

-- monad: (a -> m b) -> m a -> m b
-- functor: (a -> b) -> f a -> f b
-- applicative functor: f (a -> b) -> f a -> f b

-- class Monad m where
--     return :: a -> m a

--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y

--     fail :: String -> m a
--     fail msg = error msg

-- Example 1 for (>>=):
-- Just 1 >>= \x -> return (x+1)
-- With applicative functor
-- Just (\x -> x+1) <*> Just 1
-- With functor
-- (\x -> x+1) <$> Just 1

-- Example 2 for (>>=)，链式计算
-- Just 1 >>= \x -> return (x+1) >>= \x -> return (x*2)
-- With applicative functor
-- Just ((\x -> x*2) . (\x -> x+1)) <*> Just 1
-- With functor
-- (\x -> x*2) . (\x -> x+1) <$> Just 1


-- The game
type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft delta (left,right)
  | abs((left+delta)-right) < 4 = Just (left+delta,right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight delta (left,right)
  | abs(left-(right+delta)) < 4 = Just (left,right+delta)
  | otherwise = Nothing

-- return (0,0) >>= landLeft 1 >>= landLeft 3
