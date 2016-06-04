module Hamming where
  distance :: String -> String -> Int
  distance x y = length $ filter (\(i,j) -> i /= j) $ zip x y
