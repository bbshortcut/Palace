module TimeSpecs ( Cycle(..)
                 , Date(..)
                 , DayName(..)
                 , Frequency(..)
                 , FrequencyMode(..)
                 , Limit(..)
                 , TimeUnit(..)
                 , TimeSpec(..)
                 , nextCycle
                 , nextDay
                 ) where

import Control.Monad (liftM)
import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip,
                           addGregorianYearsClip, diffDays, fromGregorian,
                           toGregorian)
import Data.Time.Clock (DiffTime)
import Utils (today)

data Date = Date Day
          | DateTime Day DiffTime
            deriving Eq

data TimeSpec = DateSpec Date
              | PeriodSpec Date Date
                deriving Eq

data TimeUnit = Days
              | Weeks
              | Months
              | Years
                deriving (Eq, Read, Show)

data Ordinal = First
             | Second
             | Third
             | Fourth
             | Penultimate
             | Ultimate
               deriving (Eq, Read, Show)

data DayName = Monday
             | Tuesday
             | Wednesday
             | Thirsday
             | Friday
             | Saturday
             | Sunday
               deriving (Eq, Read, Show)

data Frequency = Daily
               | Weekly
               | Monthly
               | Yearly
               | Every Integer TimeUnit
               | On Ordinal DayName
                 deriving (Eq, Read, Show)

data FrequencyMode = Strict
                   | Sliding
                     deriving Eq

data Limit = Limit Day
           | Times Int
             deriving Eq

data Cycle = Cycle TimeSpec Frequency FrequencyMode
           | BornedCycle TimeSpec Frequency FrequencyMode Limit

nextCycle :: Cycle -> IO (Maybe Cycle)
nextCycle cycle = liftM (nextCycleFromToday cycle) today

nextCycleFromToday :: Cycle -> Day -> Maybe Cycle
nextCycleFromToday cycle@(Cycle (DateSpec (Date day)) freq mode) today =
    nextCycleFromDay cycle (case mode of
                              Strict    -> nextDay day freq
                              Sliding   -> nextDay today freq
                              otherwise ->
                                  error "Frequency mode not supported.")
nextCycleFromToday cycle@(BornedCycle (DateSpec (Date day)) freq mode _) today =
    nextCycleFromDay cycle (case mode of
                              Strict    -> nextDay day freq
                              Sliding   -> nextDay today freq
                              otherwise ->
                                  error "Frequency mode not supported.")

nextCycleFromDay :: Cycle -> Day -> Maybe Cycle
nextCycleFromDay (Cycle (DateSpec (Date day)) freq mode) next =
    Just $ Cycle (DateSpec (Date next)) freq mode
nextCycleFromDay (BornedCycle (DateSpec (Date day)) freq mode limit) next =
    case limit of
      Limit d ->
          if next <= d
          then Just $ BornedCycle (DateSpec (Date next)) freq mode limit
          else Nothing
      Times 0 -> Nothing
      Times n ->
          Just $ BornedCycle (DateSpec (Date next)) freq mode (Times (n - 1))
      otherwise      -> error "Limit not supported."

nextDay :: Day -> Frequency -> Day
nextDay day freq =
    case freq of
      Daily          -> addDays 1 day
      Weekly         -> addDays 7 day
      Monthly        -> addGregorianMonthsClip 1 day
      Yearly         -> addGregorianYearsClip 1 day
      Every n Days   -> addDays n day
      Every n Weeks  -> addDays (n * 7) day
      Every n Months -> addGregorianMonthsClip n day
      Every n Years  -> addGregorianYearsClip n day
      On ord name    ->
          ord `dayOf` days (firstOfNextMonth day)
                  (firstOfNextMonth (firstOfNextMonth day)) name
      otherwise      ->
          error "Frequency not supported."

dayOf :: Ordinal -> [Day] -> Day
dayOf ord =
    case ord of
      First       -> head
      Second      -> head . tail
      Third       -> head . drop 2
      Fourth      -> head . drop 3
      Penultimate -> head . tail . reverse
      Ultimate    -> last

days :: Day -> Day -> DayName -> [Day]
days start end name
    | start > end           = []
    | dayName start == name = start : days (addDays 7 start) end name
    | otherwise             =
        days (addDays (diffDayNames name (dayName start)) start) end name

firstOfNextMonth :: Day -> Day
firstOfNextMonth day = fromGregorian year month 1
    where year :: Integer
          month :: Int
          (year, month, _) = toGregorian $ addGregorianMonthsClip 1 day

diffDayNames :: DayName -> DayName -> Integer
diffDayNames nameA nameB =
    (nameRank nameA - nameRank nameB) `mod` 7
        where nameRank :: DayName -> Integer
              nameRank name =
                  case name of
                    Monday    -> 0
                    Tuesday   -> 1
                    Wednesday -> 2
                    Thirsday  -> 3
                    Friday    -> 4
                    Saturday  -> 5
                    Sunday    -> 6

dayName :: Day -> DayName
dayName day =
    case diffDays day monday `mod` 7 of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thirsday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
    where monday :: Day
          monday = fromGregorian 2009 12 14
