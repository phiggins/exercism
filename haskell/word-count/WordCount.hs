module WordCount where
  import qualified Data.Map as Map
  import Data.Char (isAlphaNum, toLower)
  import Data.List (groupBy)

  wordCount :: String -> Map.Map String Int
  wordCount = frequencies . words' . downcase
    where frequencies = foldr (\k acc -> increment k acc) Map.empty
          increment key = Map.insertWith (+) key 1
          words' s = filter (all isAlphaNum) $ groupBy (\x y -> isAlphaNum x == isAlphaNum y) s
          downcase = map toLower
