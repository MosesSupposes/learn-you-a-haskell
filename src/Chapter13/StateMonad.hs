module Chapter13.StateMonad where

import           Control.Monad.State

--  We'll say that a stateful computation is a function that takes some state and 
-- returns a value along with some new state. That function would have the following type:

-- * s -> (a,s)  

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)
pop []       = (0, [])


push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
    let ((), newStack1) = push 3 stack
        (a , newStack2) = pop newStack1
    in  pop newStack2

-- The above code for stackManip is kind of tedious since we're manually giving the state
--  to every stateful computation and storing it and then giving it to the next one. 
-- Wouldn't it be cooler if, instead of giving the stack manually to each function, 
-- we could write something like this:

-- * stackManip = do  
-- *     push 3  
-- *     a <- pop  
-- *     pop  


-- Well, using the state monad will allow us to do exactly this. With it, we will be able
-- to take stateful computations like these and use them without having to manage the state manually.

-- The Control.Monad.State module provides a newtype that wraps stateful computations. Here's its definition:
-- * newtype State s a = State { runState :: s -> (a,s) }   

-- Now that we've seen what stateful computations are about and how they can even be thought of as values with contexts, 
-- let's check out their Monad instance:
-- * instance Monad (State s) where
-- *     return x = state $ \s -> (x, s)
-- *     (State h) >>= f = state $ \s ->
-- *         let (a, newState) = h s
-- *             (State g)     = f a
-- *         in  g newState

-- Because pop and push are already stateful computations, it's easy to wrap them into a State wrapper. Watch:
pop' :: State Stack Int
pop' = state $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a : xs)


-- pop is already a stateful computation and push takes an Int and returns a stateful computation. 
-- Now we can rewrite our previous example of pushing 3 onto the stack and then popping two numbers off like this:

stackManip' :: State Stack Int
stackManip' = do
    push' 3
    pop'
    pop'
