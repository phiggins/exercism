module Robot where
  import Control.Monad.State
  import System.Random

  mkRobot :: IO StdGen
  mkRobot = newStdGen

  robotName :: StdGen -> IO String
  robotName r = return $ fst $ runState genRobotName r

  resetName = undefined

  genRobotName :: State StdGen String
  genRobotName = do
    letter1 <- randomLetter
    letter2 <- randomLetter
    digit1 <- randomDigit
    digit2 <- randomDigit
    digit3 <- randomDigit
    return [letter1, letter2, digit1, digit2, digit3]
    where randomLetter = state $ randomR ('A', 'Z')
          randomDigit = state $ randomR ('0', '9')
