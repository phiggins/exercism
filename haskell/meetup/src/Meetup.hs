module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Schedule = Teenth | First | Second | Third | Fourth | Last
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let days = map (fromGregorian year month) (dayRange schedule)
  in head $ filter (isDay weekday) days

dayRange :: Schedule -> [Int]
dayRange schedule = case schedule of
  Teenth  -> [13..19]
  First   -> [1..7]
  Second  -> [8..14]
  Third   -> [15..21]
  Fourth  -> [22..28]
  Last    -> reverse [21..31]

isDay :: Weekday -> Day -> Bool
isDay weekday day = dayIndex weekday == dayOfWeek day

dayIndex :: Weekday -> Int
dayIndex weekday = case weekday of
  Monday    -> 1
  Tuesday   -> 2
  Wednesday -> 3
  Thursday  -> 4
  Friday    -> 5
  Saturday  -> 6
  Sunday    -> 7

dayOfWeek :: Day -> Int
dayOfWeek day =
  let (_,_,n) = toWeekDate day
  in n
