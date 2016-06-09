module WordCount where
  import qualified Data.Map as Map
  import Data.Char (isAlphaNum, toLower)
  import Data.List (groupBy)

  wordCount :: String -> Map.Map String Int
  wordCount s = frequencies words
    where frequencies = foldr (\k acc -> increment k acc) Map.empty
          increment key = Map.insertWith (\_ x -> x + 1) key 1
          words = filter (all isAlphaNum) $ groupBy (\x y -> isAlphaNum x == isAlphaNum y) input
          input = map toLower s
