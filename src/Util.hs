module Util
  ( gymSleepCount,
    campingSleepCount, -- TODO: Rename
    formatDay,
    formatBirthday,
    requiresParentSignature,
  )
where

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.SharedTypes
import Types

gymSleepCount :: [Sleepover] -> Int
gymSleepCount = length . filter (\s -> s == GymSleeping)

campingSleepCount :: [Sleepover] -> Int
campingSleepCount = length . filter (== Camping)

formatBirthday :: Birthday -> String
formatBirthday (Birthday d) = formatDay d

formatDay :: Day -> String
formatDay d = formatTime defaultTimeLocale "%d.%m.%Y" d

requiresParentSignature :: Day -> Bool
requiresParentSignature birthday =
  -- this is the second day because people having their 18th birthday on the first day are fine.
  birthday >= fromGregorian 2000 10 13
