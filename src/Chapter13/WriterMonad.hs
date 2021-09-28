{-# LANGUAGE FlexibleContexts #-}

module Chapter13.WriterMonad where


import           Control.Monad.Writer           ( MonadWriter
                                                , Sum(Sum)
                                                , tell
                                                , writer
                                                )

import           Data.Monoid


-- The Writer Monad

-- The Writer monad is for values that have another value attached that acts as 
-- a sort of log value. Writer allows us to do computations while making sure 
-- that all the log values are combined into one log value that then gets attached
-- to the result.

-- Consider a function that takes a number of bandits in a gang and tells us if that's a big gang or not.

isBigGang :: Int -> Bool
isBigGang x = x > 9

-- Now, what if instead of just giving us a True or False value, we want it to also return a log string that 
-- says what it did? Well, we just make that string and return it along side our Bool:

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compared gang size to 9.")

-- Now what if we already have a value that has a log string attached to it, such as (3, "Smallish gang."), 
-- and we want to feed it to isBigGang? Consider this function:

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- Here's applyLog in action:

ex1 :: (Bool, String)
ex1 = (3, "Smallish gang.") `applyLog` isBigGang'
-- (False,"Smallish gang.Compared gang size to 9")

ex2 :: (Bool, String)
ex2 = (30, "A freaking platoon.") `applyLog` isBigGang'
-- (True,"A freaking platoon.Compared gang size to 9")  

-- Monoids to the rescue.

-- Right now, applyLog takes values of type (a,String), but is there a reason that the log has to be a String?
-- It uses ++ to append the logs, so wouldn't this work on any kind of list, not just a list of characters?
-- Sure it would. We can go ahead and change its type and implementation to this:

-- * applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])


-- Now, the log is a list. The type of values contained in the list has to be the same for the original list 
-- as well as for the list that the function returns, otherwise we wouldn't be able to use ++ to stick them together.


-- We can take it one step further and use Monoids to combine our values. Observe:

applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

-- Because the accompanying value can now be any monoid value, we no longer have to think of the tuple as a value and 
-- a log, but now we can think of it as a value with an accompanying monoid value.
-- For instance, we can have a tuple that has an item name and an item price as the monoid value.
-- We just use the Sum newtype to make sure that the prices get added as we operate with the items.
-- Here's a function that adds drink to some cowboy food:

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- We use strings to represent foods and an Int in a Sum newtype wrapper to keep track of how many cents something costs.
-- Just a reminder, doing mappend with Sum results in the wrapped values getting added together:

sumDemo :: Sum Integer
sumDemo = Sum 3 `mappend` Sum 9
-- Sum {getSum = 12} 

-- Just normally applying this function to a food wouldn't be terribly interesting right now, but using applyLog to feed a 
-- food that comes with a price itself into this function is interesting:

ex3 :: (Food, Price)
ex3 = ("beans", Sum 10) `applyLog'` addDrink
-- ("milk",Sum {getSum = 35})  

ex4 :: (Food, Price)
ex4 = ("jerky", Sum 25) `applyLog'` addDrink
-- ("whiskey",Sum {getSum = 124})  

ex5 :: (Food, Price)
ex5 = ("dogmeat", Sum 5) `applyLog'` addDrink
-- ("beer",Sum {getSum = 35})

-- Milk costs 25 cents, but if we eat it with beans that cost 10 cents, we'll end up paying 35 cents. 
-- Now it's clear how the attached value doesn't always have to be a log, it can be any monoid value and how two such 
-- values are combined into one depends on the monoid.
-- When we were doing logs, they got appended, but now, the numbers are being added up.

-- Because the value that addDrink returns is a tuple of type (Food,Price), we can feed that result to addDrink again, 
-- so that it tells us what we should drink along with our drink and how much that will cost us. Let's give it a shot:

ex6 :: (Food, Price)
ex6 = ("dogmeat", Sum 5) `applyLog'` addDrink `applyLog'` addDrink
-- ("beer",Sum {getSum = 65})  

-- The Writer type

-- Now that we've seen that a value with an attached monoid acts like a monadic value, let's examine the Monad instance for types of such values.
-- The Control.Monad.Writer module exports the Writer w a type along with its Monad instance and some useful functions for dealing with values of this type.

-- First, let's examine the type itself. To attach a monoid to a value, we just need to put them together in a tuple. The Writer w a type is just a newtype 
-- wrapper for this. Its definition is very simple:

-- * newtype Writer w a = Writer { runWriter :: (a, w) }

-- Its Monad instance is defined like so:
-- * instance (Monoid w) => Monad (Writer w) where  
-- *     return x = Writer (x, mempty)  
-- *     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Using do notation with Writer

-- Here's a simple example of using do notation with Writer to multiply two numbers:

logNumber :: (MonadWriter [String] m, Show a) => a -> m a
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: (MonadWriter [String] m, Show a, Num a) => m a
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)
-- (15, ["Got number: 3","Got number: 5","Gonna multiply these two"])


