module LeapYear where

{-
on every year that is evenly divisible by 4
  except every year that is evenly divisible by 100
    unless the year is also evenly divisible by 400
 -}

isLeapYear :: Integer -> Bool
isLeapYear x
  | x `mod` 400 == 0  = True
  | x `mod` 100 == 0  = False
  | otherwise         = x `mod` 4 == 0
