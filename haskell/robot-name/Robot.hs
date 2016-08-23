module Robot where
  import System.Random

  mkRobot :: IO StdGen
  mkRobot = newStdGen

  robotName :: StdGen -> IO String
  robotName r = return $ newRobotName r

  resetName = undefined

  newRobotName :: StdGen -> String
  newRobotName r = sequence [letter, letter, digit, digit, digit]
    where letter = randomRIO ('A', 'Z')
          digit = randomRIO ('0', '9')
