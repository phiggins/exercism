module Strain where
  keep :: (a -> Bool) -> [a] -> [a]
  keep _ [] = []
  keep f (x:xs) = case f x of
    True  -> x : (keep f xs)
    False -> keep f xs

  discard :: (a -> Bool) -> [a] -> [a]
  discard _ [] = []
  discard f (x:xs) = case f x of
    False -> x : (discard f xs)
    True  -> discard f xs
