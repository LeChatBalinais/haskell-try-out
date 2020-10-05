import CIN (addSum, valid)
import ChapterThree (exerciseA, exerciseB, exerciseD, exerciseE)
import Exponent (exponen)
import Modernize (modernize)
import Palindrome (palindrome)
import ShowDate (showDate)
import Song (song)

type Text = [Char]

double :: Integer -> Integer
double x = 2 * x

units, teens, tens :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

convert2 :: Int -> String
convert2 n
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t -2)
  | otherwise = tens !! (t -2) ++ "-" ++ units !! u
  where
    (t, u) = (n `div` 10, n `mod` 10)

convert3 :: Int -> String
convert3 n
  | h == 0 = convert2 t
  | t == 0 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where
    (h, t) = (n `div` 100, n `mod` 100)

link :: Int -> String
link h = if h < 100 then " and " else " "

convert6 :: Int -> String
convert6 n
  | m == 0 = convert3 h
  | h == 0 = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand " ++ link h ++ convert3 h
  where
    (m, h) = (n `div` 1000, n `mod` 1000)

convert :: Int -> String
convert = convert6

main :: IO ()
-- main = print (exponen 2 5)
-- main = print (showDate (11, 11, 2020))
-- main = print (addSum "34657632", valid "3465763236", valid "3465763232", valid "34657632")
-- main = print (exerciseB (((subtract 5 2) 3)))

-- main = print (exerciseB (subtract 5 2) 3)
main = print (show (exerciseE (145.3)))

-- main = print (map (show . double . double) ([1, 2] ++ [3, 4]))
-- main = print (song 4)

-- main = print (modernize "abc cba")
-- main = print (map (\n -> 2 * n + 1) [4, 3])
