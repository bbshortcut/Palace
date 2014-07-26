module VolumeDB ( addBinding
                , addVolume
                , onHost
                , setDBUp
                , testBinding
                , testVolume
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
               , Volume(..)
               , VolumeName
               , createFileForest
               , toFileTree
               , toFilePaths
               )

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

testBinding :: VolumeName -> IO ()
testBinding name = do
  host <- getHostName

  bracket (dbPath >>= connectSqlite3) disconnect $
              \ conn -> do
                 bindings <- quickQuery' conn ("SELECT * FROM bindings " ++
                                               "WHERE volume = ? " ++
                                               "AND host = ?")
                             [toSql name, toSql host]

                 when (not (null bindings)) $
                      error ("Binding for volume " ++ name ++
                             " on host " ++ host ++ " already exists.")

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

testVolume :: VolumeName -> IO ()
testVolume name = do
  bracket (dbPath >>= connectSqlite3) disconnect $
              \ conn -> do
                volumes <- quickQuery' conn
                           "SELECT * FROM volumes WHERE volume = ?"
                           [toSql name]

                when (not (null volumes)) $
                     error ("Volume " ++ name ++ " already exists.")

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

getVolumeNames :: HostName -> IO [VolumeName]
getVolumeNames host = do
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

onHost :: Bool -> [VolumeName] -> (HostName -> [VolumeName] -> IO ()) -> IO ()
onHost all names operation = do
  host <- getHostName

  if all
    then operation host =<< getVolumeNames host
    else if not (null names)
           then operation host names
           else error "Volume names are missing."
