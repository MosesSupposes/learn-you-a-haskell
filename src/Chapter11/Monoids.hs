module Chapter11.Monoids where

import qualified Data.Foldable                 as F

-- This is how the Monoid typeclass is defined:
-- * class Monoid m where  
-- *    mempty :: m  
-- *    mappend :: m -> m -> m  
-- *    mconcat :: [m] -> m  
-- *    mconcat = foldr mappend mempty

-- Because mconcat has a default implementation, we get it for free when we 
-- make something an instance of Monoid.

-- Lists are Monoids. Take a look: 
-- * instance Monoid [a] where  
-- *    mempty = []  
-- *    mappend = (++)  


-- Maybe the Monoid
-- * instance Monoid a => Monoid (Maybe a) where  
-- *    mempty = Nothing  
-- *    Nothing `mappend` m = m  
-- *    m `mappend` Nothing = m  
-- *    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2) 

-- Using monoids to fold data structures

-- * foldr :: (a -> b -> b) -> b -> [a] -> b  
-- * F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

-- Whereas foldr takes a list and folds it up, the foldr from Data.Foldable accepts any type that can be folded up, not just lists!

--  As expected, both foldr functions do the same for lists:
foldingAList :: Int
foldingAList = foldr (*) 1 [1, 2, 3]
-- 6  
foldingAList' :: Int
foldingAList' = F.foldr (*) 1 [1, 2, 3]
-- 6

foldingAMaybe :: Int
foldingAMaybe = F.foldl (+) 2 (Just 9)
-- 11

foldingAMaybe2 :: Bool
foldingAMaybe2 = F.foldr (||) False (Just True)
-- True

-- But folding over a Maybe value isn't terribly interesting, because when it comes to folding, it just acts like a list with one element 
-- if it's a Just value and as an empty list if it's Nothing. So let's examine a data structure that's a little more complex then.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Implementing foldmap is all it takes to make a type an instance of Foldable.
-- By defining this function, we get foldl and foldr for free.

-- Here's the type definition for foldMap

-- * foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  

-- And here's how we make our tree type an instance of Foldable.

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) =
        F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree :: Tree Int
testTree = Node 5
                (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty))
                (Node 9 (Node 8 Empty Empty) (Node 10 Empty Empty))

addTestTree :: Int
addTestTree = F.foldl (+) 0 testTree
-- 42  

multiplyTestTree :: Int
multiplyTestTree = F.foldl (*) 1 testTree
-- 64800  

testTreeAsList :: [Int]
testTreeAsList = F.foldMap (\x -> [x]) testTree
-- [1,3,6,5,8,9,10] 

-- As you can see, our mapping function converts each value in the tree to a Monoid (in this case a singleton list),
-- and then each list gets folded together into a single list via mappend.

-- What's cool is that all of these tricks aren't limited to trees, they work on any instance of Foldable.
