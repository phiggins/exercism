module Anagram where
  import Data.Char (toLower)
  import Data.List (sort)

  anagramsFor :: String -> [String] -> [String]
  anagramsFor x ys = filter (isAnagramOf x) ys

  isAnagramOf :: String -> String -> Bool
  isAnagramOf s t =
    let x = map toLower s
        y = map toLower t
    in x /= y && sort x == sort y
