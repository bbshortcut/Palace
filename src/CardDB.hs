module CardDB ( cards
              , delete
              , first
              , get
              , insert
              , maxId
              , nextId
              , putDBInfo
              , randomCards
              , setDBUp
              , update
              ) where

import Cards ( Card(..)
             , Id
             , isDue
             )
import Control.Exception (bracket)
import Control.Monad (liftM, when, filterM)
import Data.Maybe (fromMaybe)
import Data.Maybe (maybe)
import Database.HDBC (run, quickQuery', commit, disconnect, getTables,
                      toSql, fromSql, SqlValue (SqlNull, SqlInt32))
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Directory (getAppUserDataDirectory, doesFileExist,
                         createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.Random (newStdGen)
import Utils (getYesOrNo, unsort)

dbPath :: IO FilePath
dbPath = do
  appUserDataDir <- getAppUserDataDirectory "Palace"
  return $ appUserDataDir </> "cards.db"

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
                  commit conn
                  putStrLn ("Card " ++ show id ++ " deleted.")
                else putStrLn ("Card " ++ show id ++ " not deleted.")

cards :: IO [Card]
cards = bracket (dbPath >>= connectSqlite3) disconnect $
        \ conn -> do
            cards <- quickQuery' conn "SELECT * FROM cards" []
            return $ map cardFromSql cards

randomCards :: IO [Card]
randomCards = do
  cards <- cards
  gen   <- newStdGen
  return $ unsort gen cards

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
