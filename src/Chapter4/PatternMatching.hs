module Chapter4.PatternMatching (factorial) where

-- Factorial without pattern matching.
factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

-- Factorial with pattern matching.
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- Another example of pattern matching.
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- This is an example of a non-exhaustive pattern match.
-- The compiler warns us about this.
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

------------------------------------------------------------
-- Pattern matching on tuples:

-- w/o pattern matching.
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- With pattern matching.
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- More examples of pattern matching on tuples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

------------------------------------------------------------
-- Pattern matching on lists:

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- Rewriting the function above with syntactic sugar. Notice that the last case still has to be written the same.
tell' :: (Show a) => [a] -> String
tell' [] = "The list is empty"
tell' [x] = "The list has one element: " ++ show x
tell' [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell' (x : y : zs) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- There's also a thing called as patterns. Those are a handy way of breaking something up according to a pattern
-- and binding it to names whilst still keeping a reference to the whole thing. You do that by putting a name and
-- an @ in front of a pattern. For instance, the pattern xs@(x:y:ys). This pattern will match exactly the same
-- thing as x:y:ys but you can easily get the whole list via xs instead of repeating yourself by typing out x:y:ys in
-- the function body again. Here's a quick and dirty example:
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- capital "Dracula"  produces:
-- "The first letter of Dracula is D"