import qualified Data.Foldable as F
import           Data.Monoid

class MyMonoid m where
  mmempty :: m
  mmappend :: m -> m -> m
  mmconcat :: [m] -> m
  mmconcat = foldr mmappend mmempty

-- identity value
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- associative
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

newtype MyProduct a  = MyProduct { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => MyMonoid (MyProduct a) where
  mmempty = MyProduct 1
  MyProduct x `mmappend` MyProduct y = MyProduct (x * y)

-- 类似“短路”，我们应该把更重要的比较放在前边。
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
  (x `compare` y)

-- F.foldl (+) 2 (Just 9)
-- F.foldr (||) False (Just True)
