module DNA where

toRNA :: String -> String
toRNA s = map charToRNA s
  where charToRNA 'G' = 'C'
        charToRNA 'C' = 'G'
        charToRNA 'T' = 'A'
        charToRNA 'A' = 'U'
        charToRNA c   = c
