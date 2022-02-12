module Types (
  Point(..),
  Shape(..),
  surface
) where

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
