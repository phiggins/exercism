module DNA where
  hammingDistance :: String -> String -> Int
  hammingDistance x y = length $ filter (\(a,b) ->  a /= b) (zip x y)
