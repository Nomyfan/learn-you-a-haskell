{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Types (
  Point(..),
  Shape(..),
  surface
) where
import qualified Data.Map as Map (Map)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r)                            = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , height      :: Float
                     , phoneNumber :: String
                     , flavor      :: String
                     } deriving (Show)

-- In most scenario, we don't need type constraints in data type.
-- Instead, we should declare it in functions.
data Vector a = Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t  -> Vector t -> Vector t
(Vector i1 j1 k1) `vPlus` (Vector i2 j2 k2) = Vector (i1 + i2) (j1 + j2) (k1 + k2)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i1 j1 k1) `scalarMult` (Vector i2 j2 k2) = (i1 * i2) + (j1 * j2) + (k1 * k2)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Partial applied type
type IntMap = Map.Map Int

-- Infix
infixr 5 :-:
-- Recursive data type
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- let a = 3 :-: 4 :-: 5 :-: Empty
-- let b = 6 :-: 7 :-: Empty
-- -> 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))

data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = TreeNode x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (TreeNode a left right)
  | x == a = TreeNode x left right
  | x < a = TreeNode a (treeInsert x left) right
  | x > a = TreeNode a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (TreeNode a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums

-- 定义一个typeclass，里面包含需要实现的函数
class MyEq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  -- In this way, we only need to implement === or /===
  x === y = not (x /== y)
  x /== y = not (x === y)

data TrafficLight = Red | Yellow | Green deriving (Show, Read)

instance MyEq TrafficLight where
  Red === Red       = True
  Yellow === Yellow = True
  Green === Green   = True
  _ === _           = False

instance (MyEq m) => MyEq (Maybe m) where
  Just x === Just y   = x === y
  Nothing === Nothing = True

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id -- id :: a -> a

instance YesNo (Maybe a) where
  yesno (Just _ ) = True
  yesno Nothing   = False

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

-- yesno $ length []
-- yesno []
-- yesno ""

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

class MyFunctor f where
  fnmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  fnmap = map

instance MyFunctor Maybe where
  fnmap f (Just x) = Just (f x)
  fnmap f Nothing  = Nothing

instance MyFunctor Tree where
  fnmap f EmptyTree = EmptyTree
  fnmap f (TreeNode x left right) = TreeNode (f x) (fnmap f left) (fnmap f right)

-- fnmap (*4) $ foldr treeInsert EmptyTree [5,7,3,2,1,7]

class Tofu t where
  tofu :: j a -> t a j

newtype Frank a b = Frank (b a) deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

-- tofu (Just 'a') :: Frank Char Maybe

data Barry t k p = Barry {yabba :: p, dabba :: t k}
instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
