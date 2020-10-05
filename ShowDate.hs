module ShowDate (showDate) where

type Date = (Int, Int, Int)

months :: [[Char]]
months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

endings :: [[Char]]
endings = ["st", "nd", "rd"]

showDay :: Int -> String
showDay day
  | (rmdr <= 3) && not ((day > 10) && (day < 20)) = (show day) ++ (endings !! (rmdr -1))
  | otherwise = (show day) ++ "th"
  where
    rmdr = rem day 10

showDate :: Date -> String
showDate date = (showDay day) ++ " " ++ (months !! (month - 1)) ++ " " ++ (show year)
  where
    (day, month, year) = date
