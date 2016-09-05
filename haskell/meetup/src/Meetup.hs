module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- The task is to create the data types `Weekday` and
-- `Schedule`, and implement the function `meetupDay`.

data Schedule = Teenth | First | Second | Third | Fourth | Last
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Teenth weekday year month = teenth $ daysOfMonth year month weekday
meetupDay First weekday year month = head $ daysOfMonth year month weekday
meetupDay Second weekday year month = last . take 2 $ daysOfMonth year month weekday
meetupDay Third weekday year month = last . take 3 $ daysOfMonth year month weekday
meetupDay Fourth weekday year month = last . take 4 $ daysOfMonth year month weekday
meetupDay Last weekday year month = last $ daysOfMonth year month weekday

teenth :: [Day] -> Day
teenth xs = head $ filter (\x -> let (_,_,n) = toGregorian x in n > 12 && n < 20) xs

daysOfMonth :: Integer -> Int -> Weekday -> [Day]
daysOfMonth year month weekday = filter (isDay weekday) (allDaysOfMonth year month)

isDay :: Weekday -> Day -> Bool
isDay Monday day      = dayOfWeek day == 1
isDay Tuesday day     = dayOfWeek day == 2
isDay Wednesday day   = dayOfWeek day == 3
isDay Thursday day    = dayOfWeek day == 4
isDay Friday day      = dayOfWeek day == 5
isDay Saturday day    = dayOfWeek day == 6
isDay Sunday day      = dayOfWeek day == 7

dayOfWeek :: Day -> Int
dayOfWeek day =
  let (_,_,n) = toWeekDate day
  in n

allDaysOfMonth :: Integer -> Int -> [Day]
allDaysOfMonth year month = map (fromGregorian year month) [1..31]
