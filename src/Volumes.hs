module Volumes ( Binding(..)
               , Volume(..)
               , VolumeName
               , backupVolume
               , checkVolume
               , createBinding
               , createFileForest
               , createVolume
               , printVolume
               , restoreVolume
               , toFilePaths
               , toFileTree
               ) where

import Control.Monad (forM_, filterM, liftM)
import Data.List (sort)
import Data.Tree (Tree(..), Forest, unfoldForestM_BF)
import Network.BSD (HostName, getHostName)
import System.Directory (createDirectoryIfMissing,
                         doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import Utils ( FileName
             , Timestamp
             , doesPathExist
             , getDirectoryContents'
             , getTimestamp
             , getTimestampsInDirectory
             , getYesOrNo
             , latestTimestamp
             , takeFileName'
             , timestampsOlderThan
             )

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

type VolumeName = String

data Volume = Volume VolumeName FileForest

data Binding = Binding VolumeName HostName FilePath FilePath

createBinding :: VolumeName -> FilePath -> FilePath -> IO Binding
createBinding name vpoint bpoint = do
  host <- getHostName

  return $ Binding name host vpoint bpoint

createVolume :: VolumeName -> FilePath -> IO Volume
createVolume name vpoint = do
  liftM (Volume name) $ createFileForest vpoint

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

checkVolume :: Volume -> Binding -> IO ()
checkVolume = undefined

printVolume :: Volume -> Binding -> IO ()
printVolume (Volume name forest) (Binding _ _ vpoint bpoint) = do
  putStrLn ("Volume: " ++ name)
  putStrLn ("VPoint: " ++ vpoint)
  putStrLn ("BPoint: " ++ bpoint)
  putStrLn "Paths:"
  mapM_ putStrLn $ concatMap toFilePaths forest

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
