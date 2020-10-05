module Modernize (modernize) where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize s = [toUpper (head s)] ++ tail s

modernize :: String -> String
modernize = unwords . map capitalize . words