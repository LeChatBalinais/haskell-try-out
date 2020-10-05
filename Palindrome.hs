module Palindrome (palindrome) where

import Data.Char (isAlpha, toLower)

answer :: Bool -> String
answer b
  | b == True = "Yes!"
  | otherwise = "No!"

isPalindrome :: String -> Bool
isPalindrome s = (((map toLower) . (filter isAlpha)) s) == ((reverse . (map toLower) . (filter isAlpha)) s)

palindrome :: IO ()
palindrome = do
  putStrLn "Enter a string:"
  xs <- getLine
  putStrLn (answer (isPalindrome xs))