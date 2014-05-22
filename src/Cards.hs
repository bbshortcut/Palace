module Cards ( ask
             , cards
             , create
             , demote
             , edit
             , export
             , first
             , get
             , insert
             , isDue
             , maxId
             , nextId
             , promote
             , put
             , putDBInfo
             , delete
             , setDBUp
             , update)
    where

import Control.Exception (bracket)
import Control.Monad (liftM, when, filterM)
import Data.Maybe (fromMaybe, maybe)
import Data.Time.Calendar (Day, addDays)
import Database.HDBC (run, quickQuery', commit, disconnect, getTables,
                      toSql, fromSql, SqlValue (SqlNull, SqlInt32))
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Directory (getAppUserDataDirectory, doesFileExist,
                         createDirectoryIfMissing)
import System.FilePath (combine, takeDirectory)
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

dbPath :: IO FilePath
dbPath = do
  appUserDataDir <- getAppUserDataDirectory "hcards"
  return $ combine appUserDataDir "cards.db"

createDB :: IO ()
createDB =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                  run conn ("CREATE TABLE cards (id INTEGER NOT NULL, " ++
                            "question VARCHAR, answer VARCHAR, day DATE, " ++
                            "frequency VARCHAR)") []
                  commit conn

setDBUp :: IO ()
setDBUp = do
  createDirectoryIfMissing True . takeDirectory =<< dbPath
  bracket (dbPath >>= connectSqlite3) disconnect $
              \ conn -> do
                  tables <- getTables conn
                  when ("cards" `notElem` tables) createDB

maxId :: IO (Maybe Id)
maxId =
    bracket (dbPath >>= connectSqlite3) disconnect $
            \ conn -> do
                m <- quickQuery' conn "SELECT MAX(id) FROM cards" []
                return (case head (head m) of
                          SqlNull -> Nothing
                          value   -> Just . fromSql $ value)

nextId :: IO Id
nextId = liftM (maybe 0 succ) maxId

cardToSql :: Card -> [SqlValue]
cardToSql (Card i r v d f) =
    [toSql i, toSql r, toSql v, toSql d, toSql $ show f]

cardFromSql :: [SqlValue] -> Card
cardFromSql (i : r : v : d : p : []) =
    Card (fromSql i) (fromSql r) (fromSql v) (fromSql d) (read (fromSql p))
cardFromSql _ = error "Can't get a Card from that list."

insert :: Card -> IO ()
insert card =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                    run conn ("INSERT INTO cards (id, question, " ++
                              "answer, day, frequency) VALUES " ++
                              "(?, ?, ?, ?, ?)") $ cardToSql card
                    commit conn
                    putStrLn ("Card " ++ show (_cId card) ++ " inserted.")

get :: Id -> IO (Maybe Card)
get id = bracket (dbPath >>= connectSqlite3) disconnect $
         \ conn -> do
             card <- quickQuery' conn "SELECT * FROM cards WHERE id = ?"
                     [toSql id]
             return (case card of
                       []      -> Nothing
                       [value] -> Just . cardFromSql $ value)

delete :: Id -> IO ()
delete id = bracket (dbPath >>= connectSqlite3) disconnect $
            \ conn -> do
              isConfirmed <- getYesOrNo ("Are you sure you want to delete " ++
                                         "card " ++ show id)

              if isConfirmed
                then do
                  run conn "DELETE FROM cards WHERE ID=?" [toSql id]
                  putStrLn ("Card " ++ show id ++ " deleted.")
                else putStrLn ("Card " ++ show id ++ " not deleted.")

cards :: IO [Card]
cards = bracket (dbPath >>= connectSqlite3) disconnect $
        \ conn -> do
            cards <- quickQuery' conn "SELECT * FROM cards" []
            return $ map cardFromSql cards

first :: [Card] -> IO (Maybe Card)
first cards = do
  dueCards <- filterM isDue cards

  return (case dueCards of
            []     -> Nothing
            (c:_) -> Just c)

update :: Card -> Card -> IO ()
update old new = bracket (dbPath >>= connectSqlite3) disconnect $
                 \ conn -> do
                     run conn ("UPDATE cards SET question=?, answer=?, " ++
                               "day=?, frequency=? WHERE id=?") $
                           drop 1 (cardToSql new) ++ [head $ cardToSql old]
                     commit conn
                     putStrLn ("Card " ++ show (_cId old) ++ " updated.")

putDBInfo :: IO ()
putDBInfo = do
  cards <- cards
  dueCards <- filterM isDue cards
  putStrLn ("There are " ++ show (length cards) ++ " cards of which " ++
            show (length dueCards) ++ " are due today.")
