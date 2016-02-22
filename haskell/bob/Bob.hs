module Bob where
  import Data.Char (isLetter, isLower, isSpace, isUpper)

  responseFor :: String -> String
  responseFor s
    | isEmpty s     = "Fine. Be that way!"
    | isShout s     = "Whoa, chill out!"
    | isQuestion s  = "Sure."
    | otherwise     = "Whatever."
    where   isEmpty = all isSpace
            isQuestion s = (last s) == '?'
            isShout s = any isLetter s && not (any isLower s)
