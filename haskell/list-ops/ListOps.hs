module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ a []     = a
foldl' f a (x:xs) = seq (f a x) (foldl' f (f a x) xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ a []      = a
foldr f a (x:xs)  = f x (foldr f a xs)

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse xs  = foldl' (\x y -> y:x) [] xs

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x y -> f x : y) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x y -> if f x then x:y else y) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (\x y -> x : y) ys xs

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs
