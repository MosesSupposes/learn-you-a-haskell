module Chapter11.Functors where

import           Data.Char
import           Data.List

-- This is how IO can be defined as a Functor
-- * instance Functor IO where
-- *    fmap f action = do
-- *        result <- action
-- *        return (f result)


-- The next two functions are equivalent. The second one
-- makes use of `fmap`.

sayBackwards :: IO ()
sayBackwards = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"
    putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

sayBackwards' :: IO ()
sayBackwards' = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

-- Here's an example of using fmap with function composition.

dashedBackwards :: IO ()
dashedBackwards = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

-- Functions are Functors too! Take a look:
--  * instance Functor ((->) r) where
--  *   fmap f g = (\x -> f (g x))

-- This is just function composition! So we can replace the defintion above with: 
-- * instance Functor ((->) r) where  
-- * 	fmap = (.)  
