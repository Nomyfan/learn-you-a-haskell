import           Control.Monad (MonadPlus (mzero), guard)
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

-- do notation，用来解决在需要多个Monad时，存在的嵌套问题。比如
withoutDoNotation :: Maybe String
withoutDoNotation = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

withDoNotation :: Maybe String
withDoNotation = do
  -- x <- Nothing :: Maybe Int
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- 当do块里pattern matching失败，那么会调用fail方法

--
myGuard :: (MonadPlus m) => Bool -> m ()
myGuard True  = return ()
myGuard False = mzero

-- [1..50] >>= (\x -> myGuard ('7' `elem` show x) >> return x)
-- Written in do notation
sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  -- 以下代码都是>>=之后的
  guard ('7' `elem` show x)
  return x

-- A knight's quest
type KnightPos = (Int, Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

moveKnightUsingFilter :: KnightPos -> [KnightPos]
moveKnightUsingFilter (c,r) = filter onBoard
  [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
  ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = pure start >>= moveKnight >>= moveKnight >>= moveKnight
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- ↓ Left identity(from Monad Raws)
-- in3 start = moveKnight start >>= moveKnight >>= moveKnight
-- in3 start = [start] >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- Monad raws
-- Left identity: return m >>= f SAME_AS f m
-- Right identity: m >>= return SAME_AS m
-- Associativity: (m >>= f) >>= g SAME_AS m >>= (\x -> f x >>= g)
