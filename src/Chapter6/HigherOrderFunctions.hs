module Chapter6.HigherOrderFunctions where

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = g
  where
    g x y = f y x

-- Here's an even simpler way of defining the `flip` function.
-- This example takes advantage of partial application.
-- Ex: backwardsSubtract = flip' (-)
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' f xs = case xs of
  [] -> []
  (x : xs) -> f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x : xs)
  | predicate x = x : filter' predicate xs
  | otherwise = filter' predicate xs

-- Here's the elegant quicksort algorithm again. Only this time, we use
-- a HOF instead of a list comprehension
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

-- Here's our flip function again. Only this time, we make use of a lambda
-- for increased readability
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-----------------------------------------------------------------

-- Folds

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldl (\acc cur -> if x == cur then True else acc) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

--Of course, we could have implemented this function with a left fold too.
-- It would be map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs, but the
-- thing is that the ++ function is much more expensive than :, so we
-- usually use right folds when we're building up new lists from a list.

-- One big difference is that right folds work on infinite lists, whereas left
-- ones don't!

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if acc > x then acc else x)

reverse' :: [a] -> [a]
reverse' = foldl (\acc cur -> cur : acc) []

product' :: Num a => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred = foldr (\x acc -> if pred x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\x _ -> x)

-- TODO: Implement foldl, foldr, foldl1, foldr1, scanl, scanr, scanl1, and scanr1.

-------------------------------------------------------------------------------------------

-- Function application and composition

-- We can rewrite the following expression with better readability
-- using function application.
-- Here's the ugly version:

example1 :: Int
example1 = sum (filter (> 10) (map (* 2) [2 .. 10]))

-- Here's the more elegant version using function application:
example1' :: Int
example1' = sum $ filter (> 10) $ map (* 2) [2 .. 10]

-- Note that function application is right associative because it has
-- the lowest operator precedence.

-- Here's the type signature for function application
--  * (a -> b) -> a -> b

-- But apart from getting rid of parentheses, $ means that function application
-- can be treated just like another function. That way, we can, for instance,
-- map function application over a list of functions.

example2 :: Floating a => [a]
example2 = map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

-- This call produces:
-- [7.0,30.0,9.0,1.7320508075688772]

-- Function composition

-- * (.) :: (b -> c) -> (a -> b) -> a -> c

-- * f . g = \x -> f (g x)

-- Function composition is right-associative, so we can compose many functions at a time.
-- The expression f (g (z x)) is equivalent to (f . g . z) x.
-- With that in mind, we can turn:

example3 :: [Int]
example3 = map (\xs -> negate (sum (tail xs))) [[1 .. 5], [3 .. 6], [1 .. 7]]

-- into:

example3' :: [Int]
example3' = map (negate . sum . tail) [[1 .. 5], [3 .. 6], [1 .. 7]]

-- Here's an example of combining function application with function composition
-- for the purpose of simplifying a bunch of nested parentheses.

-- Here's the ugly example:

example4 :: [Int]
example4 = replicate 100 (product (map (* 3) (zipWith max [1, 2, 3, 4, 5] [4, 5, 6, 7, 8])))

-- Here's the more elegant example that combines function application with function
-- composition and partial application.

example4' :: [Int]
example4' = replicate 100 . product . map (* 3) . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]
