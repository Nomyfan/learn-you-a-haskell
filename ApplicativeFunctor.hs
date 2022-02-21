import           Control.Applicative (liftA2)
data MyMaybe a = MyNothing | MyJust a

instance Functor MyMaybe where
  fmap f (MyJust m) = MyJust (f m)
  fmap f _          = MyNothing

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _          = MyNothing
    (MyJust f) <*> something = fmap f something -- reuse Functor

-- (+) <$> (+3) <*> (*100) $ 5
-- (fmap (+) (+3)) <*> (*100) $ 5
-- ((+).(+3)) <*> (*100) $ 5
-- (\x -> ((+).(+3)) x ((*100) x)) 5

--liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
--liftA2 f a b = f <$> a <*> b

-- Array<Maybe<T>> -> Maybe<Array<T>>
sequenceA_1 :: (Applicative f) => [f a] -> f [a]
sequenceA_1 []     = pure []
sequenceA_1 (x:xs) = (:) <$> x <*> (sequenceA_1 xs) -- 这个形式有利于理解以下这个例子
-- sequenceA [(+3),(+2),(+1)] 3

sequenceA_2 :: (Applicative f) => [f a] -> f [a]
sequenceA_2 = foldr (liftA2 (:)) (pure [])
