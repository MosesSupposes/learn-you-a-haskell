module Chapter9.InputOutput
    ( helloChicken
    , complementUser
    ) where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )

helloChicken :: IO ()
helloChicken = putStrLn "Hello chicken!"

complementUser :: IO ()
complementUser = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")


-- Let bindings don't need an `in` clause inside `do` blocks.
getEmotionalStatus :: IO ()
getEmotionalStatus = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- `return` is the opposite of `<-`. 
-- `return` boxes a value into a monad while `<-` unboxes a value and binds it to a name.
foreverBackwards :: IO ()
foreverBackwards = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            foreverBackwards
    where reverseWords = unwords . map reverse . words


-- Herer's a contrived demonstration of `return` and `<-` working in tandem.
-- This shows how they're indeed opposites.
hellYeah :: IO ()
hellYeah = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-- The above is redundant and can instead be expressed as: 
hellYeah' :: IO ()
hellYeah' = do
    let a = "hell"
    let b = "yeah!"
    putStrLn $ a ++ " " ++ b
