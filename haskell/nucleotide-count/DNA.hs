module DNA where
  import qualified Data.Map as Map

  count :: Char -> String -> Int
  count c s = length $ filter (\i -> i == nucleotideOrError c) (cleanString s)

  nucleotideCounts :: String -> Map.Map Char Int
  nucleotideCounts s = Map.fromList $ map (\c -> (c, count c string)) "ATCG"
    where string = cleanString s

  nucleotideOrError :: Char -> Char
  nucleotideOrError c
    | elem c "ATCG" = c
    | otherwise     = error $ concat ["invalid nucleotide '", [c], "'"]

  cleanString :: String -> String
  cleanString s = map nucleotideOrError s
