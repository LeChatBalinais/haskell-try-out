module CIN (addSum, valid) where

type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum c = c ++ show (sum ((map getDigit) c))

valid :: CIN -> Bool
valid c = c == (addSum . (take 8)) c
