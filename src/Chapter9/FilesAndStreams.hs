module Chapter9.FilesAndStreams
    ( printGirlfriend
    , printGirlfriendCaps
    , addTodo
    ) where

import           Data.Char                      ( toUpper )
import           System.IO

respondPalindromes :: String -> String
respondPalindromes =
    unlines
        . map
              (\xs ->
                  if isPalindrome xs then "palindrome" else "not a palindrome"
              )
        . lines
    where isPalindrome xs = xs == reverse xs

-- The following three functions are equivalent.

printGirlfriend :: IO ()
printGirlfriend = do
    handle   <- openFile "src/Chapter9/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

printGirlfriend' :: IO ()
printGirlfriend' = do
    withFile
        "src/Chapter9/girlfriend.txt"
        ReadMode
        (\handle -> do
            contents <- hGetContents handle
            putStr contents
        )

printGirlfriend'' :: IO ()
printGirlfriend'' = do
    contents <- readFile "src/Chapter9/girlfriend.txt"
    putStr contents

printGirlfriendCaps = do
    contents <- readFile "src/Chapter9/girlfriend.txt"
    writeFile "src/Chapter9/girlfriendcaps.txt" (map toUpper contents)

-- Here's an implementation of `withFile`.

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

addTodo :: IO ()
addTodo = do
    todoItem <- getLine
    appendFile "src/Chapter9/todo.txt" (todoItem ++ "\n")


