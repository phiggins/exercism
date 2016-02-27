module Phone where
  import Data.Char (isDigit)

  areaCode :: String -> String
  areaCode s = take 3 $ number s

  prettyPrint :: String -> String
  prettyPrint s = concat ["(", areaCode n, ") ", middleThree, "-", lastFour]
    where n = number s
          middleThree = take 3 $ drop 3 n
          lastFour  = drop 6 n

  number :: String -> String
  number s
    | length n == 10  = n
    | length n == 11 && head n == '1' = tail n
    | otherwise       = "0000000000"
    where n = filter isDigit s
