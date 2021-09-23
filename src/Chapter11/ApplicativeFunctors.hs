module Chapter11.ApplicativeFunctors where

-- Problem: How to apply a function inside a Functor to a value in another Functor?

-- Solution: One way would be to unwrap the Functor with the function inside it and 
--map it over the Functor with the value. Take a look:

naiveSolution :: Maybe Int
naiveSolution =
  let functionFunctor = Just (* 3)
      intFunctor      = Just 5
  in  case functionFunctor of
        Just f  -> fmap f intFunctor
        Nothing -> Nothing

-- Just 15

-- What if there was a more general, abstract way of doing the above?
-- Well there is! Behold the Applicative Functor!

-- Here's how its typeclass is defined
-- * class (Functor f) => Applicative f where  
-- *    pure :: a -> f a  
-- *    (<*>) :: f (a -> b) -> f a -> f b 


-- Let's take a look at the Applicative instance implementation for Maybe.

-- * instance Applicative Maybe where  
-- *    pure = Just  
-- *    Nothing <*> _ = Nothing  
-- *    (Just f) <*> something = fmap f something  

-- OK cool great. Let's give this a whirl.

betterSolution :: Maybe Int
betterSolution = pure (* 3) <*> Just 5

otherExamples :: (Maybe Int, Maybe Int, Maybe Int, Maybe String, Maybe String)
otherExamples =
  let x1 = Just (+ 3) <*> Just 9
      -- Just 12  
      x2 = pure (+ 3) <*> Just 10
      -- Just 13  
      x3 = pure (+ 3) <*> Just 9
      -- Just 12  
      x4 = Just (++ "hahah") <*> Nothing
      -- Nothing  
      x5 = Nothing <*> Just "woot"
      -- Nothing 
  in  (x1, x2, x3, x4, x5)

chainedApplicative :: Maybe Int
chainedApplicative = pure (+) <*> Just 3 <*> Just 5
-- Just 8

-- Instead of writing pure f <*> x <*> y <*> ..., we can write fmap f x <*> y <*> .... 
-- This is why Control.Applicative exports a function called <$>, which is just fmap as 
-- an infix operator. Here's how it's defined:
-- * (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
-- * f <$> x = fmap f x  

-- With this in mind, we can rewrite the example above as followed: 

chainedApplicative' :: Maybe Int
chainedApplicative' = (+) <$> Just 3 <*> Just 5
-- Just 8

-- Or we could write it in a more primitive fashion: 

chainedApplicative'' :: Maybe Int
chainedApplicative'' = fmap (+) (Just 3) <*> Just 5

-- The previos version (chainedApplicative') is the preferred nethod over this more primitive version.
-- Here's why: 
-- By using <$>, the applicative style really shines, because now if we want to apply a function f 
-- between three applicative functors, we can write f <$> x <*> y <*> z. If the parameters weren't 
-- applicative functors but normal values, we'd write f x y z
-- Here's an example:

johnTravolta :: Maybe String
johnTravolta = (++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"

-- If we weren't using functors and we were just using plain values, the above could be rewritten as this:

johnTravolta' :: String
johnTravolta' = (++) "johntra" "volta"

-- Do you see the magic? Just sprinkle in some <$> and <*> and you've got some hokus pokus when dealing with Functors!

-- Lists (actually the list type constructor, []) are applicative functors. What a suprise! 
-- Here's how [] is an instance of Applicative:

-- * instance Applicative [] where  
-- *   pure x = [x]  
-- *   fs <*> xs = [f x | f <- fs, x <- xs]  

-- Due to the fact that <*> is implemented using a list comprehension,
-- the resulting list has every possible combination of applying a function from the left list to a value in the right one.
-- So, if there are 3 functions on the left of the <*> and 3 values on the right, we would get back a list of 9 elemnts.
-- Here's an example: 

listApplicative :: [Int]
listApplicative = [(* 0), (+ 100), (^ 2)] <*> [1, 2, 3]
-- [0,0,0,101,102,103,1,4,9] 

-- If we have a list of functions that take two parameters, we can apply those functions between two lists. 

listApplicativeMultipleLists :: [Int]
listApplicativeMultipleLists = [(+), (*)] <*> [1, 2] <*> [3, 4]
-- [4,5,5,6,3,4,6,8] 

-- Because <*> is left-associative, [(+),(*)] <*> [1,2] happens first, resulting in a list that's the same as [(1+),(2+),(1*),(2*)], 
-- because every function on the left gets applied to every value on the right. 

-- Using the applicative style with lists is fun! Watch:

listApplicative2 :: [String]
listApplicative2 = (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- Using the applicative style on lists is often a good replacement for list comprehensions.
-- In the second chapter, we wanted to see all the possible products of [2,5,10] and [8,10,11], so we did this:

beforeWeLearnedAboutApplicatives :: [Int]
beforeWeLearnedAboutApplicatives =
  [ x * y | x <- [2, 5, 10], y <- [8, 10, 11] ]
-- [16,20,22,40,50,55,80,100,110] 

-- We're just drawing from two lists and applying a function between every combination of elements. 
-- This can be done in the applicative style as well:

afterWeLearnedAboutApplicatives :: [Int]
afterWeLearnedAboutApplicatives = (*) <$> [2, 5, 10] <*> [8, 10, 11]
-- [16,20,22,40,50,55,80,100,110]   

-- If we wanted all possible products of those two lists that are more than 50, we'd just do:
afterWeLearnedAboutApplicatives2 :: [Int]
afterWeLearnedAboutApplicatives2 =
  filter (> 50) $ (*) <$> [2, 5, 10] <*> [8, 10, 11]

