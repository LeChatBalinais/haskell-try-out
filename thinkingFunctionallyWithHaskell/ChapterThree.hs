module ChapterThree (exerciseA, exerciseB, exerciseC, exerciseD, exerciseE, exerciseF) where

type Interval = (Integer, Integer)

exerciseA :: (Num a) => a -> a -> a
exerciseA = flip (-)

exerciseB :: (Fractional a, Integral b) => a -> b -> a
exerciseB a b = if 0 <= b then a ^ b else 1 / (a ^ (negate b))

exerciseC :: Integral a => a -> a -> a
exerciseC x y = floor (fromIntegral x / fromIntegral y)

exerciseD :: Float -> Integer
exerciseD = read . (takeWhile (/= '.')) . show

bound :: Float -> Interval
bound x = (0, until above (* 2) 1)
  where
    above n = x < fromInteger ((n * n))

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (fromInteger (p * p) <= x) then (p, n) else (m, p)
  where
    p = (m + n) `div` 2

exerciseE :: Float -> Integer
exerciseE x = fst (until unit (shrink x) (bound x))
  where
    unit (m, n) = m + 1 == n

-- data Nat = Zero | Succ !Nat

-- instance Eq Nat where
--   Zero == Zero = True
--   Zero == Succ n = False
--   Succ m == Zero = False
--   Succ m == Succ n = (m == n)

-- instance Show Nat where
--   show Zero = "Zero"
--   show (Succ Zero) = "Succ Zero"
--   show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

-- instance Num Nat where
--   m + Zero = m
--   m + Succ n = Succ (m + n)
--   m * Zero = Zero
--   m * (Succ n) = m * n + m
--   abs n = n
--   signum Zero = Zero
--   signum (Succ n) = Succ Zero

--   m - Zero = m
--   Zero - Succ n = Zero
--   Succ m - Succ n = m - n

--   fromInteger x
--     | x <= 0 = Zero
--     | otherwise = Succ (fromInteger (x -1))

-- instance Ord Nat where
--   Zero < Zero = False
--   Zero < Succ n = True
--   Succ m < Zero = False
--   Succ m < Succ n = (m < n)

-- divMod :: Nat -> Nat -> (Nat, Nat)
-- divMod x y =
--   if x < y
--     then (Zero, x)
--     else (Succ q, r)
--   where
--     (q, r) = divMod (x - y) y

exerciseF :: Float -> Float
exerciseF x = until goodenough improve x
  where
    goodenough y = abs (y * y - x) < eps * x
    eps = 0.000001
    improve y = (y + x / y) / 2