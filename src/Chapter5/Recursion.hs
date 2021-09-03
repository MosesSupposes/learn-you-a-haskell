module Chapter5.Recursion where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot find maximum of an empty list"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

-- Here's an alternative way of defining the maximum function.
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Cannot find maximum of an empty list"
maximum'' [x] = x
maximum'' (x : xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' amount _
  | amount <= 0 = []
take' _ [] = []
take' amount (x : xs) = x : take' (amount - 1) xs

-- This definition is the same as above; the difference is that is showcases
-- using case expressions with guards
take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' amount xs = case (amount, xs) of
  (amount, _)
    | amount <= 0 -> []
  (_, []) -> []
  (amount, x : xs) -> x : take' (amount - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted