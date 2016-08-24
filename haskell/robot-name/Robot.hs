module Robot where
  import Data.IORef
  import System.Random

  type Robot = IORef String

  mkRobot :: IO Robot
  mkRobot = do
    name <- newRobotName
    newIORef name

  robotName :: Robot -> IO String
  robotName = readIORef

  resetName :: Robot -> IO ()
  resetName r = do
    newName <- newRobotName
    writeIORef r newName

  newRobotName :: IO String
  newRobotName = sequence [letter, letter, digit, digit, digit]
    where letter = randomRIO ('A', 'Z')
          digit = randomRIO ('0', '9')
