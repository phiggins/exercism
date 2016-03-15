module School where
  import qualified Data.Map as Map
  import Data.List (insert)

  type School = Map.Map Int [String]

  sorted :: School -> [(Int, [String])]
  sorted school = Map.toAscList school

  grade :: Int -> School -> [String]
  grade n school = case Map.lookup n school of
    Just k -> k
    Nothing -> []

  add :: Int -> String -> School -> School
  add n name school = Map.insertWith (\x xs -> insert (head x) xs)  n [name] school

  empty :: School
  empty = Map.empty
