module Volumes (setDBUp, VolumeName, Options(..), defaults,
               createBinding, addBinding, createVolume, addVolume,
               onHost, withVolumes, backupVolume, printVolume, restoreVolume)
        where

import Control.Monad (forM_, liftM, when, filterM)
import Data.List (sort, find)
import Data.Maybe (isJust, mapMaybe, fromJust, catMaybes)
import Data.Time.Calendar (Day)
import Data.Tree (Tree(..), Forest, unfoldForestM_BF)
import Database.HDBC (run, prepare, executeMany, quickQuery', commit,
                      disconnect, getTables, toSql, fromSql, SqlValue (SqlNull))
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.BSD (HostName, getHostName)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing,
                         doesDirectoryExist,
                         doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import Utils (FileName, Timestamp, addTreeToForest, doesPathExist,
              getDirectoryContents', getTimestamp, getTimestampsInDirectory,
              getYesOrNo, latestTimestamp, takeFileName', timestampsOlderThan)

type FileTree = Tree FileName
type FileForest = Forest FileName

toFilePaths :: FileTree -> [FilePath]
toFilePaths (Node node []) = [node]
toFilePaths (Node node subForest) =
    map (node </>) (concatMap toFilePaths subForest)

toFileTree :: FilePath -> Maybe FileTree
toFileTree "" = Nothing
toFileTree path = Just $ Node parent subForest
    where nodes :: [FilePath]
          nodes = splitDirectories path

          parent :: FilePath
          parent = head nodes

          subForest :: FileForest
          subForest =
              case toFileTree (joinPath $ tail nodes) of
                Nothing -> []
                Just tree -> [tree]

walkDirectory :: FilePath -> IO (FileName, [FilePath])
walkDirectory path = do
  directoryExists <- doesDirectoryExist path

  if directoryExists
    then do
      stepInto <- getYesOrNo ("Step into " ++ path)

      if stepInto
        then do
          contents <- getDirectoryContents' path
          filteredContents <- filterM (\ name -> getYesOrNo $ "Include " ++
                                       path </> name) $ sort contents

          return (takeFileName' path, map (path </>) filteredContents)
        else return (takeFileName' path, [])
    else return (takeFileName' path, [])

createFileForest :: FilePath -> IO FileForest
createFileForest path = do
  directoryExists <- doesDirectoryExist path

  if directoryExists
    then do
      contents <- getDirectoryContents' path
      filteredContents <- filterM (\ name -> getYesOrNo $ "Include " ++
                                             path </> name) $ sort contents
      unfoldForestM_BF walkDirectory $ map (path </>) filteredContents
    else error ("Path " ++ path ++ " not found.")

dbPath :: IO FilePath
dbPath = do
  appUserDataDir <- getAppUserDataDirectory "Palace"
  return $ appUserDataDir </> "volumes.db"

createDB :: IO ()
createDB = do
  conn <- dbPath >>= connectSqlite3
  run conn ("CREATE TABLE volumes " ++
            "(volume VARCHAR NOT NULL, path VARCHAR NOT NULL)") []
  run conn ("CREATE TABLE bindings " ++
            "(volume VARCHAR NOT NULL, host VARCHAR NOT NULL, " ++
            "vpoint VARCHAR NOT NULL, bpoint VARCHAR NOT NULL)") []
  commit conn
  disconnect conn

setDBUp :: IO ()
setDBUp = do
  path <- dbPath

  createDirectoryIfMissing True $ takeDirectory path

  conn <- connectSqlite3 path
  tables <- getTables conn
  disconnect conn

  when ("volumes" `notElem` tables) createDB

type VolumeName = String

data Volume = Volume VolumeName FileForest

data Binding = Binding VolumeName HostName FilePath FilePath

data Options =
    Options { optName   :: Maybe VolumeName
            , optVPoint :: Maybe FilePath
            , optBPoint :: Maybe FilePath
            , optAll    :: Bool
            , optNames  :: [VolumeName]
            } deriving Show

defaults :: Options
defaults =
    Options { optName   = Nothing
            , optVPoint = Nothing
            , optBPoint = Nothing
            , optAll    = False
            , optNames  = []
            }

testBinding :: Options -> IO ()
testBinding opts = do
  host <- getHostName

  if isJust . optName $ opts
    then do
      let volume = fromJust . optName $ opts

      conn <- dbPath >>= connectSqlite3
      bindings <- quickQuery' conn ("SELECT * FROM bindings WHERE volume = ? " ++
                                    "AND host = ?") [toSql volume, toSql host]
      disconnect conn

      when (not (null bindings)) $
           error ("Binding for volume " ++ volume ++ " on host " ++ host ++
                  " already exists.")
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
addBinding (Binding volume host vpoint bpoint) = do
  conn <- dbPath >>= connectSqlite3

  run conn ("INSERT INTO bindings (volume, host, vpoint, bpoint) " ++
            "VALUES (?, ?, ?, ?)")
      [toSql volume, toSql host, toSql vpoint, toSql bpoint]
  commit conn
  disconnect conn

getBindings :: HostName -> IO [Binding]
getBindings host = do
  conn <- dbPath >>= connectSqlite3
  bindings <- quickQuery' conn
              "SELECT * FROM bindings WHERE host = ?" [toSql host]
  disconnect conn
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

        conn    <- dbPath >>= connectSqlite3
        volumes <- quickQuery' conn "SELECT * FROM volumes WHERE volume = ?"
                   [toSql volume]
        disconnect conn

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
addVolume (Volume volume forest) = do
  conn <- dbPath >>= connectSqlite3
  request <- prepare conn
             "INSERT INTO volumes (volume, path) VALUES (?, ?)"

  executeMany request $
              map (\ path -> [toSql volume, toSql path]) $
                  sort $ concatMap toFilePaths forest
  commit conn
  disconnect conn

getVolume :: VolumeName -> IO (Maybe Volume)
getVolume volume = do
  conn <- dbPath >>= connectSqlite3
  paths <- quickQuery' conn "SELECT path FROM volumes WHERE volume = ?"
           [toSql volume]
  disconnect conn

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

backupPathCmd :: Binding -> Maybe Timestamp -> Timestamp -> FilePath -> String
backupPathCmd (Binding volume _ vpoint bpoint) Nothing newTimestamp path =
    "rsync -ax " ++ vpoint </> path ++ " " ++
    bpoint </> volume </> newTimestamp </> dir
        where dir :: FilePath
              dir = takeDirectory path
backupPathCmd (Binding volume _ vpoint bpoint)
                  (Just oldTimestamp) newTimestamp path =
    "rsync -ax --link-dest=" ++
    bpoint </> volume </> oldTimestamp </> dir ++ " " ++
    vpoint </> path ++ " " ++
    bpoint </> volume </> newTimestamp </> dir
        where dir :: FilePath
              dir = takeDirectory path

backupPath :: Binding -> Maybe Timestamp -> Timestamp -> FilePath -> IO ()
backupPath binding@(Binding volume _ vpoint bpoint)
           oldTimestamp newTimestamp path = do
  putStr $ "Backuping [" ++ volume ++ "]/" ++ path ++ "... "
  hFlush stdout
  pathExists <- doesPathExist $ vpoint </> path

  if pathExists
    then do
      createDirectoryIfMissing True $ (</>) (bpoint </> volume </> newTimestamp) $ takeDirectory path
      callCommand $ backupPathCmd binding oldTimestamp newTimestamp path
      putStrLn "done."
      hFlush stdout
    else do
      putStrLn "failed."
      hFlush stdout
      error $ "Path " ++ vpoint </> path ++ " not found."

backupVolume :: Volume -> Binding -> IO ()
backupVolume (Volume volume forest) binding@(Binding _ _ vpoint bpoint) = do
  newTimestamp <- getTimestamp
  timestamps <- getTimestampsInDirectory $ bpoint </> volume

  forM_ (concatMap toFilePaths forest) $
            backupPath binding (latestTimestamp timestamps) newTimestamp
  mapM_ (removeDirectoryRecursive . (</>) bpoint . (</>) volume) $
        timestampsOlderThan 31 (maximum timestamps) timestamps

printVolume :: Volume -> Binding -> IO ()
printVolume volume binding = error "Not implemented."

restorePathCmd :: Binding -> Timestamp -> FilePath -> String
restorePathCmd (Binding volume _ vpoint bpoint) timestamp path =
    "rsync -ax --delete " ++
    bpoint </> volume </> timestamp </> path ++ " " ++
    vpoint </> (takeDirectory path)

restorePath :: Binding -> Maybe Timestamp -> FilePath -> IO ()
restorePath _ Nothing _ =
    error "No timestamp given."
restorePath binding@(Binding volume _ vpoint bpoint)
                (Just timestamp) path = do
  putStr $ "Restoring [" ++ volume ++ "]/" ++ path ++ "... "
  hFlush stdout
  pathExists <- doesPathExist $
                (</>) bpoint . (</>) volume . (</>) timestamp $ path

  if pathExists
    then do
      createDirectoryIfMissing True $ (</>) vpoint $ takeDirectory path
      callCommand $ restorePathCmd binding timestamp path
      putStrLn "done."
      hFlush stdout
    else do
      putStrLn "failed."
      hFlush stdout
      error ("Path " ++
             bpoint </> volume </> timestamp </> path ++
             " not found.")

restoreVolume :: Volume -> Binding -> IO ()
restoreVolume (Volume volume forest) binding@(Binding _ _ vpoint bpoint) = do
  timestamps <- getTimestampsInDirectory $ bpoint </> volume

  forM_ (concatMap toFilePaths forest) $
            (restorePath binding (latestTimestamp timestamps))
