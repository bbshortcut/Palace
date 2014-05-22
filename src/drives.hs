import Data.Maybe (isJust, fromJust)
import Drives (setDBUp, VolumeName, Options(..), defaults,
               createBinding, addBinding, createVolume, addVolume,
               onHost, withVolumes, backupVolume, printVolume, restoreVolume)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

volumeOptions :: [OptDescr (Options -> Options)]
volumeOptions =
    [ Option "n" ["name"]
                 (ReqArg (\ name opts ->
                              opts { optName = Just name }) "NAME")
                 "volume name NAME"
    ]

bindingOptions :: [OptDescr (Options -> Options)]
bindingOptions =
    volumeOptions ++
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
    [ Option "n" ["name"]
                 (ReqArg (\ name opts ->
                              opts { optNames = name : optNames opts }) "NAME")
                 "volume name NAME"
    , Option "a" ["all"]
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
    "create" : _  -> do
              (opts, _) <- parseOpt bindingOptions $ tail args
              volume    <- createVolume opts
              binding   <- createBinding opts

              addVolume volume
              addBinding binding
    "backup" : _  -> do
              (opts, _) <- parseOpt allOptions $ tail args

              onHost opts $ withVolumes backupVolume
    "info" : _    -> do
              (opts, _) <- parseOpt allOptions $ tail args

              onHost opts $ withVolumes printVolume
    "restore" : _ -> do
              (opts, _) <- parseOpt allOptions $ tail args

              onHost opts $ withVolumes restoreVolume
    _             ->
        ioError (userError ("No command passed\n" ++
                 "Usage: " ++ progName ++
                 " [create|backup|restore] [OPTIONS...]\n"))
