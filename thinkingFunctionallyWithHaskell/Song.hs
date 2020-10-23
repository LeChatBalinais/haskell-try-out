module Song (song) where

units :: [[Char]]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

man :: Int -> String
man n
  | n == 1 = units !! n ++ " man"
  | otherwise = units !! n ++ " men"

manEnumeration :: Int -> String
manEnumeration n
  | n == 1 = man n
  | n <= 9 = man n ++ ", " ++ manEnumeration (n -1)
  | otherwise = ""

song :: Int -> String
song n
  | n == 1 = man n ++ " went to mow\nWent to mow a meadow\n" ++ manEnumeration n ++ " and his dog\n" ++ "Went to mow a meadow\n\n"
  | n <= 9 = song (n -1) ++ man n ++ " went to mow\nWent to mow a meadow\n" ++ manEnumeration n ++ " and his dog\n" ++ "Went to mow a meadow\n\n"
  | otherwise = ""
