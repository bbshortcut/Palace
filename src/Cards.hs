module Cards ( Card(..)
             , Id
             , ask
             , create
             , demote
             , edit
             , export
             , isDue
             , promote
             , put
             ) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, addDays)
import System.IO (hFlush, stdout)
import TimeSpecs (Frequency(..), TimeUnit(..), nextDay)
import Utils (getAnswer, getAnswerWithPrefill, getYesOrNo, itemPred,
              itemSucc, today)

type Id = Int
type Side = String

data Card = Card { _cId :: Id
                 , _cRecto :: Side
                 , _cVerso :: Side
                 , _cDay :: Day
                 , _cFrequency :: Frequency
                 } deriving Show

create :: Id -> IO Card
create id = do
  r <- getAnswer "What is the question"
  v <- getAnswer "What is the answer"
  d <- today
  return $ Card id r v (addDays 1 d) Daily

put :: Card -> IO ()
put card = putStrLn $ show (_cId card) ++ ". " ++ _cRecto card

export :: Card -> IO ()
export card =
    putStrLn $ show (_cId card) ++ " | " ++ _cRecto card ++
                 " | " ++ _cVerso card

ask :: Card -> IO Bool
ask card = do
  putStrLn ("Asknig for card " ++ show (_cId card) ++ ".")
  putSide "Question:" (_cRecto card)
  isKnown <- getYesOrNo "Have you the answer"
  putSide "Answer:" (_cVerso card)

  if isKnown
    then getYesOrNo "Was your answer correct"
    else return False
      where putSide :: String -> String -> IO ()
            putSide intro side =
                putStrLn (intro ++ "\n") >> putStrLn ("\t" ++ side ++ "\n")

edit :: Card -> IO Card
edit card = do
  recto <- getAnswerWithPrefill (_cRecto card) "What is the question"
  verso <- getAnswerWithPrefill (_cVerso card) "What is the answer"
  return card { _cRecto = recto, _cVerso = verso }

isDue :: Card -> IO Bool
isDue card = liftM (_cDay card <=) today

frequencies = [ Daily
              , Every 2 Days
              , Every 3 Days
              , Weekly
              , Every 2 Weeks
              , Monthly
              , Every 2 Months
              , Every 3 Months
              , Every 6 Months
              , Yearly
              ]

freqNeighbor :: Bool -> Frequency -> Maybe Frequency
freqNeighbor pred freq
    | pred      = frequencies `itemPred` freq
    | otherwise = frequencies `itemSucc` freq

setFreq :: Maybe Frequency -> Card -> IO Card
setFreq freq card = do
  let freq' = fromMaybe (_cFrequency card) freq
  newD <- liftM (`nextDay` freq') today
  return card { _cDay = newD, _cFrequency = freq' }

demote :: Card -> IO Card
demote card = setFreq (freqNeighbor True $ _cFrequency card) card

promote :: Card -> IO Card
promote card = setFreq (freqNeighbor False $ _cFrequency card) card
