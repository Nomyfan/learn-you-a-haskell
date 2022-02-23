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
