module Chapter4.Guards where

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2

-- This example demonstrates that you can have multiple variables in the where clause.
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

-- This is a contrived example that mixes guards with pattern matching.
lucky7 :: (Num a, Eq a) => [a] -> String
lucky7 [] = "Please provide me with at least 3 numbers"
lucky7 (x : y : z : _)
  | x == 7 = "You were lucky on the first try."
  | y == 7 = "You were lucky on the second try."
  | z == 7 = "You were lucky on the third try."
  | otherwise = "You ran out of chances. You're quite unlucky."
lucky7 (x : xs) = "Please provide me with at least 3 numbers"

--------------------------------------------------------------------------------

-- More examples of the the `where` clause in functions

-- We could have done this pattern matching directly in the function's parameters
-- (it would have been shorter and clearer actually) but this just goes to show
-- that it's possible to do it in where bindings as well.
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

-- This is a safer version of the above definition
safeInitials :: String -> String -> Either String String
safeInitials firstname lastname
  | firstname /= "" && lastname /= "" = Right ([f] ++ "." ++ [l] ++ ".")
  | otherwise = Left "You must provide both a first name and a last name"
  where
    (f : _) = firstname
    (l : _) = lastname

-- Calculate multiple bmi's given a list of weight and height pairs.
-- Notice that this `where` clause specifies a function instead of a hard-coded value.
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- `where` bindings can also be nested. It's a common idiom to make a function
-- and define some helper function in its where clause and then to give those
-- functions helper functions as well, each with its own where clause.
