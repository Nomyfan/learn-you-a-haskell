-- data P b a = P (a, b)
newtype P b a = P (a, b) deriving (Show)
instance Functor (P c) where
  fmap f (P (x,y)) = P (f x, y)

-- (+1) <$> P(1,2)

-- 用record syntax的好处是可以对值进行extract
newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- getPair $ (+1) <$> Pair (1,2)

-- data和newtype的区别
newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
-- newtype是lazy的，且只能有一个构造函数且只能有一个field，这里还把这个field忽略了，
-- 那么Haskell推断出这个pattern不需要进行evaluate。这时给它传undefined不会报错。
-- 如果把这个忽略拿掉，那么又会报错。
helloMe (CoolBool _) = "hello"
-- helloMe undefined
