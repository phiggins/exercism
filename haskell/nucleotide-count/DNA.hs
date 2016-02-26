module DNA where
  import qualified Data.Map as Map
  import Data.List ((\\))

  count :: Char -> String -> Int
  count c s
    | not (validNucleotides s)  = badNucleotides s
    | not (validNucleotide c)   = badNucleotide c
    | otherwise                 = length $ filter (\i -> i == c) s

  nucleotideCounts :: String -> Map.Map Char Int
  nucleotideCounts s
    | validNucleotides s  = Map.fromList $ map (\c -> (c, count c s)) nucleotides
    | otherwise           = badNucleotides s

  badNucleotides :: String -> a
  badNucleotides s = badNucleotide $ head $ s \\ nucleotides

  badNucleotide :: Char -> a
  badNucleotide c = error $ concat ["invalid nucleotide '", [c], "'"]

  validNucleotides :: String -> Bool
  validNucleotides = all validNucleotide

  validNucleotide :: Char -> Bool
  validNucleotide c = elem c nucleotides

  nucleotides :: String
  nucleotides = "ACGT"
