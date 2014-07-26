module Main where

import Data.Maybe (fromMaybe)
import VolumeDB (setDBUp, addBinding, addVolume, onHost, testBinding,
                 testVolume, withVolumes)
import Volumes (VolumeName, backupVolume, checkVolume, createBinding,
                createVolume, printVolume, restoreVolume)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

data Options =
    Options { optVPoint :: Maybe FilePath
            , optBPoint :: Maybe FilePath
            , optAll    :: Bool
            }

defaults :: Options
defaults =
    Options { optVPoint = Nothing
            , optBPoint = Nothing
            , optAll    = False
            }

bindingOptions :: [OptDescr (Options -> Options)]
bindingOptions =
    [ Option "v" ["vpoint"]
                 (ReqArg (\ vpoint opts ->
                              opts { optVPoint = Just vpoint }) "VPOINT")
                 "volume point VPOINT"
    , Option "b" ["bpoint"]
                 (ReqArg (\ bpoint opts ->
                              opts { optBPoint = Just bpoint }) "BPOINT")
                 "backup point BPOINT"
    ]

allOptions :: [OptDescr (Options -> Options)]
allOptions =
    [ Option "a" ["all"]
                 (NoArg (\ opts ->
                             opts { optAll = True }))
                 "all volume names"
    ]

parseOpt :: [OptDescr (Options -> Options)]
         -> [String] -> IO (Options, [String])
parseOpt options args = do
  progName <- getProgName

  case getOpt Permute options args of
    (o, n, [])   -> return (foldl (flip id) defaults o, n)
    (_, _, errs) ->
        ioError (userError (concat errs ++
                                   usageInfo (header progName) options))
    where header :: String -> String
          header name = "Usage: " ++ name ++ " " ++
                                   head args ++ " [OPTIONS...]"

main = do
  progName <- getProgName
  args <- getArgs
  setDBUp

  case args of
    "backup" : _  -> do
              (opts, names) <- parseOpt allOptions $ tail args

              onHost (optAll opts) names $ withVolumes backupVolume
    "check" : _   -> do
              (opts, names) <- parseOpt allOptions $ tail args

              onHost (optAll opts) names $ withVolumes checkVolume
    "create" : _  -> do
              (opts, names) <- parseOpt bindingOptions $ tail args

              let name = if length names == 1
                           then head names
                           else createUsage progName
                  vpoint = fromMaybe (createUsage progName) $ optVPoint opts
                  bpoint = fromMaybe (createUsage progName) $ optBPoint opts

              testBinding name
              testVolume name

              addBinding =<< createBinding name vpoint bpoint
              addVolume =<< createVolume name vpoint
    "info" : _    -> do
              (opts, names) <- parseOpt allOptions $ tail args

              onHost (optAll opts) names $ withVolumes printVolume
    "restore" : _ -> do
              (opts, names) <- parseOpt allOptions $ tail args

              onHost (optAll opts) names $ withVolumes restoreVolume
    _             -> usage progName
    where createUsage name =
              error ("Bad create command\n." ++
                     "Usage: " ++ name ++
                     " create --vpoint <VPOINT> " ++
                     "--bpoint <BPOINT> <NAME>\n")
          usage name =
              ioError (userError ("No command passed\n" ++
                                  "Usage: " ++ name ++
                                  " [backup|check|create|info|restore] " ++
                                  "[OPTIONS...] [NAMES...]\n"))
