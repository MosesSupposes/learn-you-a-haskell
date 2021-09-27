module Chapter12.Monads where

-- Recap:

-- * fmap :: (Functor f) => (a -> b) -> f a -> f b
-- * (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  

-- This is the type signature for `bind`, the main operator for dealing with Monads.
-- * (>>=) :: (Monad m) => m a -> (a -> mb) -> m b

-- Monads are just applicative functors that support bind.

-- Here's the type class for Monad:
-- * class Monad m where  
-- *     return :: a -> m a  

-- *     (>>=) :: m a -> (a -> m b) -> m b  

-- *     (>>) :: m a -> m b -> m b  
-- *     x >> y = x >>= \_ -> y  

-- *     fail :: String -> m a  
-- *     fail msg = error msg  

-- Now that we know what the Monad type class looks like, let's take a look at how Maybe is an instance of Monad!

-- * instance Monad Maybe where  
-- *     return x = Just x  
-- *     Nothing >>= f = Nothing  
-- *     Just x >>= f  = f x  
-- *     fail _ = Nothing 

-- We can play around with Maybe as a monad:

maybeAsMonad :: (Maybe String, Maybe Int, Maybe Int)
maybeAsMonad =
    let ex1 = return "WHAT"
        ex2 = Just 9 >>= \x -> return (x * 10)
        ex3 = Nothing >>= \x -> return (x * 10)
    in  (ex1, ex2, ex3)
-- ("WHAT", Just 90, Nothing)

exclaimed3 :: Maybe String
exclaimed3 = Just 3 >>= (\x -> Just (show x ++ "!"))
-- Just "3!"  

-- This example produces the same output as the one above, except it chains >>= inside the lambda.
exclaimed3' :: Maybe String
exclaimed3' = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"

-- To save us from writing all these annoying lambdas, Haskell gives us do notation. It allows us 
-- to write the previous piece of code like this:

exclaimed3'' :: Maybe String
exclaimed3'' = do
    x <- Just 3
    y <- Just "!"
    return (show x ++ y)

-- In do notation, when we bind monadic values to names, we can utilize pattern matching, just like in let expressions and function parameters. 
-- Here's an example of pattern matching in a do expression:

justH :: Maybe Char
justH = do
    (x : xs) <- Just "hello"
    return x
-- Just "h"

-- Here's the Monad instance for Lists:
-- * instance Monad [] where  
-- *     return x = [x]  
-- *     xs >>= f = concat (map f xs)  
-- *     fail _ = [] 

-- Just like with Maybe values, we can chain several lists with >>=
listOfTuples :: [(Int, Char)]
listOfTuples = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]  

-- Here's the previous expression rewritten with do notation.
listOfTuples' :: [(Int, Char)]
listOfTuples' = do
    n  <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

-- Using do notation on lists is very similar to list comprehensions. Take a look: 
listOfTuples'' :: [(Int, Char)]
listOfTuples'' = [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]  

-- In fact, list comprehensions are just syntactic sugar for using lists as monads. 

-- The MonadPlus type class is for monads that can also act as monoids. Here's its definition:
-- * class Monad m => MonadPlus m where  
-- *     mzero :: m a  
-- *     mplus :: m a -> m a -> m a  

-- mzero is synonymous to mempty from the Monoid type class and mplus corresponds to mappend.
-- Because lists are monoids as well as monads, they can be made an instance of this type class:
-- * instance MonadPlus [] where  
-- *     mzero = []  
-- *     mplus = (++)  

-- Remember the function composition operator? Take a look
-- * (.) :: (b -> c) -> (a -> b) -> (a -> c)  
-- * f . g = (\x -> f (g x))  

-- How can we compose functions that return monadic values? The . operator won't work.
-- Introducing the Fish operator:

-- *  (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
-- *  f <=< g = (\x -> g x >>= f)  


(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) f g = (\x -> g x >>= f)
-- We can use the fish operator to compose monadic functions. Here's an example: 

fishCompose :: [Int]
fishCompose =
    let f x = [x, -x]
        g x = [x * 3, x * 2]
        h = f <=< g
    in  h 3
-- [9,-9,6,-6] 

