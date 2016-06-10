module WordCount where
  import qualified Data.Map as Map
  import Data.Char (isAlphaNum, toLower)
  import Data.List (groupBy)

  wordCount :: String -> Map.Map String Int
  wordCount = frequencies . (wordsBy isAlphaNum) . (map toLower)
    where frequencies = foldr increment Map.empty
          increment key = Map.insertWith (+) key 1
          wordsBy f = filter (all f) . groupBy (\x y -> f x == f y)
