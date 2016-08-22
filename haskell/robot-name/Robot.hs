module Robot where
  import System.Random

  mkRobot :: IO StdGen
  mkRobot = newStdGen

  robotName :: StdGen -> IO String
  robotName r = return $ genRobotName r

  resetName = undefined

  genRobotName :: StdGen -> String
  genRobotName r = fst $ foldl (\(s,g') f -> let (l,g'') = f g' in (l:s,g'')) ("", r) [randomLetter, randomLetter, randomDigit, randomDigit, randomDigit]
    where randomLetter = randomR ('A', 'Z')
          randomDigit = randomR ('0', '9')
