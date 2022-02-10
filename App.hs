{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs

-- [Guards]
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

-- [let in]是表达式，相当于创建了一个闭包给in里的表达式使用

-- [case of]是语法糖
describeList :: [a] -> String
-- describeList xs = "The list is " ++ what xs
--   where what [] = "empty."
--         what [_] = "a singleton list."
--         what xs = "a longer list"
describeList xs = "The list is " ++ what xs
  where what xs = case xs of []  -> "empty."
                             [x] -> "a singleton list."
                             xs  -> "a longer list."

-- Recursive
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

-- HOF
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- applyTwice (+3) 10
-- applyTwice (++ " HAHA") "HEY"
-- applyTwice ("HAHA " ++) "HEY"
-- applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (+) [4,2,5,6] [2,6,2,3]
-- zipWith' max [6,3,2,1] [7,3,1,5]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = let g x y = f y x in g
-- flip' zip [1,2,3,4,5] "hello"
-- zipWith (flip' div) [2,2..] [10,8,6,4,2]

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs
-- map' (+2) [1,2,3]
-- map' (++ "!") ["BIFF", "BANG", "POW"]
-- map' (map' (^2)) [[1,2],[3,4,5,6],[7,8]]
-- ((map' (*) [0..]) !! 4) 5

filter' :: (a -> Bool ) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs
-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

collatz :: (Integral a) => a -> [a]
collatz n
  | n == 1 = [1]
  | even n = n:collatz (n `div` 2)
  | odd n = n:collatz (n*3 + 1)

-- Lambda
-- pattern matching比较弱，不能多模式匹配
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x:acc) []
reverse' = foldl (flip (:)) []

-- scanl (flip (:)) [] [3,2,1]

sqrtSum :: Int
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Function application with $
-- ($) :: (a -> b) -> a -> b
withoutDollar = sum (filter (>10) (map (*2) [2..10]))
withDollar = sum $ filter (>10) $ map (*2) [2..10]
-- 个人觉得F#的pip操作符更舒服
-- 如果是用F#的|>，那么应该是这样的
-- map (*2) [2..10] |> filter (>10) |> sum

-- 还是函数
-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- Function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]
--
-- we can turn this
-- sum (replicate 5 (max 6.7 8.9))
-- into
-- sum . replicate 5 . max 6.7 $ 8.9

-- 这里例子比较好的说明composition的好处
-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn x = ceiling $ negate $ tan $ cos $ max 50 $ x
fn = ceiling . negate . tan . cos . max 50
