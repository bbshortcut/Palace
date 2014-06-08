module VolumeDB ( addBinding
                , addVolume
               , createBinding
                , createVolume
                , onHost
                , setDBUp
                , withVolumes
                ) where

import Control.Exception (bracket)
import Control.Monad (forM_, liftM, when)
import Data.List (sort, find)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Database.HDBC (run, prepare, executeMany, quickQuery', commit,
                      disconnect, getTables, toSql, fromSql, SqlValue (SqlNull))
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.BSD (HostName, getHostName)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath ((</>), takeDirectory)
import Utils (addTreeToForest)
import Volumes ( Binding(..)
               , Options(..)
               , Volume(..)
               , VolumeName
               , createFileForest
               , toFileTree
               , toFilePaths)

dbPath :: IO FilePath
dbPath = do
  liftM (</> "volumes.db") $ getAppUserDataDirectory "Palace"

createDB :: IO ()
createDB = bracket (dbPath >>= connectSqlite3) disconnect $
           \ conn -> do
             run conn ("CREATE TABLE volumes " ++
                       "(volume VARCHAR NOT NULL, path VARCHAR NOT NULL)") []
             run conn ("CREATE TABLE bindings " ++
                       "(volume VARCHAR NOT NULL, host VARCHAR NOT NULL, " ++
                       "vpoint VARCHAR NOT NULL, bpoint VARCHAR NOT NULL)") []
             commit conn

setDBUp :: IO ()
setDBUp = do
  path <- dbPath

  createDirectoryIfMissing True $ takeDirectory path

  bracket (connectSqlite3 path) disconnect $
              \ conn -> do
                tables <- getTables conn

                when ("volumes" `notElem` tables) createDB

testBinding :: Options -> IO ()
testBinding opts = do
  host <- getHostName

  if isJust . optName $ opts
    then do
      let volume = fromJust . optName $ opts

      bracket (dbPath >>= connectSqlite3) disconnect $
                  \ conn -> do
                     bindings <- quickQuery' conn ("SELECT * FROM bindings " ++
                                                   "WHERE volume = ? " ++
                                                   "AND host = ?")
                                 [toSql volume, toSql host]

                     when (not (null bindings)) $
                          error ("Binding for volume " ++ volume ++
                                 " on host " ++ host ++ " already exists.")
    else error "Options are missing."

createBinding :: Options -> IO Binding
createBinding opts = do
  testBinding opts

  host <- getHostName

  if (isJust . optName $ opts) &&
     (isJust . optVPoint $ opts) &&
     (isJust . optBPoint $ opts)
    then return $ Binding (fromJust . optName $ opts) host
             (fromJust . optVPoint $ opts) (fromJust . optBPoint $ opts)
    else error "Options are missing."

addBinding :: Binding -> IO ()
addBinding (Binding volume host vpoint bpoint) =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                  run conn ("INSERT INTO bindings (volume, host, " ++
                            "vpoint, bpoint) VALUES (?, ?, ?, ?)")
                          [ toSql volume, toSql host, toSql vpoint
                          , toSql bpoint]
                  commit conn

getBindings :: HostName -> IO [Binding]
getBindings host =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                  bindings <- quickQuery' conn
                              "SELECT * FROM bindings WHERE host = ?"
                              [toSql host]

                  return $ map makeBinding bindings
      where makeBinding :: [SqlValue] -> Binding
            makeBinding [volume, host, vpoint, bpoint] =
                Binding (fromSql volume) (fromSql host)
                            (fromSql vpoint) (fromSql bpoint)

getBinding :: VolumeName -> HostName -> IO (Maybe Binding)
getBinding volume host = do
  bindings <- getBindings host

  return $ find (\ (Binding name _ _ _) -> volume == name) bindings

testVolume :: Options -> IO ()
testVolume opts =
    if isJust . optName $ opts
      then do
        let volume = fromJust . optName $ opts

        bracket (dbPath >>= connectSqlite3) disconnect $
                    \ conn -> do
                      volumes <- quickQuery' conn
                                 "SELECT * FROM volumes WHERE volume = ?"
                                 [toSql volume]

                      when (not (null volumes)) $
                           error ("Volume " ++ volume ++ " already exists.")
      else error "Options are missing."

createVolume :: Options -> IO Volume
createVolume opts = do
  testVolume opts

  if (isJust . optName $ opts) &&
     (isJust . optVPoint $ opts)
    then do
      forest <- createFileForest $ fromJust . optVPoint $ opts
      return $ Volume (fromJust . optName $ opts) forest
    else error "Options are missing."

addVolume :: Volume -> IO ()
addVolume (Volume volume forest) =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                  request <- prepare conn
                             "INSERT INTO volumes (volume, path) VALUES (?, ?)"

                  executeMany request $
                              map (\ path -> [toSql volume, toSql path]) $
                                  sort $ concatMap toFilePaths forest
                  commit conn

getVolume :: VolumeName -> IO (Maybe Volume)
getVolume volume =
    bracket (dbPath >>= connectSqlite3) disconnect $
                \ conn -> do
                  paths <- quickQuery' conn
                           "SELECT path FROM volumes WHERE volume = ?"
                           [toSql volume]

                  case paths of
                    []        -> return Nothing
                    otherwise ->
                        return . Just . Volume volume .
                               foldl addTreeToForest [] . mapMaybe toFileTree $
                               sort . map (fromSql :: SqlValue -> String) $
                               concat paths

getVolumes :: HostName -> IO [VolumeName]
getVolumes host = do
  bindings <- getBindings host

  return . map (\ (Binding name _ _ _) -> name) $ bindings

withVolumes :: (Volume -> Binding -> IO ())
            -> HostName -> [VolumeName] -> IO ()
withVolumes operation host volumes = do
  host     <- getHostName
  mVolumes <- mapM getVolume volumes

  forM_ (catMaybes mVolumes) $
            \ volume@(Volume name _) ->
                do
                  mBinding <- getBinding name host

                  case mBinding of
                    Nothing      ->
                        error ("Volume " ++ name ++
                               " not bound on host " ++ host ++ ".")
                    Just binding -> operation volume binding

onHost :: Options -> (HostName -> [VolumeName] -> IO ()) -> IO ()
onHost opts operation = do
  host <- getHostName

  if optAll opts
    then do
      names <- getVolumes host
      operation host names
    else if not (null . optNames $ opts)
           then operation host (optNames opts)
           else error "Options are missing."

