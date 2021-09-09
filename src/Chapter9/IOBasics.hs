module Chapter9.IOBasics
    ( helloChicken
    , complementUser
    , mapSequence
    ) where

import           Control.Monad
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
        b = "yeah!"
    putStrLn $ a ++ " " ++ b

printUntilSpace :: IO ()
printUntilSpace = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        printUntilSpace


-- The following two functions are identical.

get3Lines :: IO ()
get3Lines = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

get3Lines' :: IO ()
get3Lines' = do
    ls <- sequence [getLine, getLine, getLine]
    print ls


-- The following two functions are identical. 

mapSequence :: IO [()]
mapSequence = sequence $ map print [1, 2, 3, 4, 5]

mapSequence' :: IO [()]
mapSequence' = mapM print [1, 2, 3, 4, 5]

foreverUpperCased :: IO ()
foreverUpperCased = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

pickColors :: IO [()]
pickColors = do
    colors <- forM
        [1, 2, 3, 4]
        (\a -> do
            putStrLn
                $  "Which color do you associate with the number "
                ++ show a
                ++ "?"
            getLine
        )
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
