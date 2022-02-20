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
