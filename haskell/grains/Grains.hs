module Grains where

  square :: Integer -> Integer
  square x = 2 ^ (x-1)

  total :: Integer
  total = foldl (\acc i -> acc + (square i)) 0 [1..64]
