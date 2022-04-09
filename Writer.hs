import           Control.Monad.Writer

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)

-- 这个形式比较好理解do notation
multWithLog2 :: Writer [String] Int
multWithLog2 = logNumber 3 >>= (\a -> logNumber 5 >>= (\b -> return (a*b)))

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList ([] ++)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return b
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

-- mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
