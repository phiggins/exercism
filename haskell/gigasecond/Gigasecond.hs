module Gigasecond where
  import Data.Time.Clock (UTCTime, addUTCTime)

  fromDay :: UTCTime -> UTCTime
  fromDay date = addUTCTime (10^9) date
