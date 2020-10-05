module Exponent (exponen) where

exponen :: Integer -> Integer -> Integer
exponen x n
  | n == 0 = 1
  | n == 1 = x
  | even n = (exponen x (n `div` 2)) * (exponen x (n `div` 2))
  | odd n = x * exponen x (n - 1)
