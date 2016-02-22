module SumOfMultiples where
  sumOfMultiples :: [Integer] -> Integer -> Integer
  sumOfMultiples xs y = sum $ filter (\i -> divisibleBy i xs) [1..y-1]
    where divisibleBy n ms = any (\m -> n `mod` m == 0) ms

  sumOfMultiplesDefault :: Integer -> Integer
  sumOfMultiplesDefault = sumOfMultiples [3, 5]
