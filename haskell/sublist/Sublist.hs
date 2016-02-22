module Sublist where

  import Data.List

  data Sublist  = Equal
                | Sublist
                | Superlist
                | Unequal
    deriving (Eq, Show)

  sublist       :: (Eq a) => [a] -> [a] -> Sublist
  sublist xs ys
    | xs == ys                = Equal
    | isInfixOf xs ys         = Sublist
    | isInfixOf ys xs         = Superlist
    | otherwise               = Unequal
