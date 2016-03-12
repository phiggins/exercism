module School where
  import Data.List (groupBy, sort)

  type Student = (Int, String)
  type School = [Student]

  sorted :: School -> [(Int, [String])]
  sorted school = sort . map gradeAndNames $ groupedStudents school
    where groupedStudents = groupBy (\x y -> fst x == fst y) . sort
          gradeFromList = fst . head
          namesFromList = sort . map snd
          gradeAndNames list = (gradeFromList list, namesFromList list)

  grade :: Int -> School -> [String]
  grade n school = map snd $ filter (\x -> fst x == n) school

  add :: Int -> String -> School -> School
  add n name school = (n, name) : school

  empty :: School
  empty = []
