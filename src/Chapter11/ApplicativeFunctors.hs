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
