module Main where

import CardDB (cards, delete, first, get, insert, maxId, nextId, putDBInfo,
               setDBUp, update)
import Cards (ask, create, demote, edit, export, isDue, promote, put)
import Control.Monad (filterM, forM_, liftM, when)
import Data.Maybe (catMaybes, fromJust)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

data Options =
    Options { optAll    :: Bool
            } deriving Show

defaults :: Options
defaults =
    Options { optAll = False
            }

allOptions :: [OptDescr (Options -> Options)]
allOptions =
    [ Option "a" ["all"]
                 (NoArg (\ opts ->
                             opts { optAll = True }))
                 "all due cards"
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
  args <- getArgs
  setDBUp

  case args of
    ["add"]        -> insert =<< create =<< nextId
    ("edit":ids)   -> do
              max <- liftM fromJust maxId
              let ids' = if null ids then [max] else map read ids
              mCards <- mapM get ids'

              forM_ (catMaybes mCards) $
                        \ card -> update card =<< edit card
    ["export"]     -> mapM_ export =<< cards
    ["info"]       -> putDBInfo
    ["list"]       -> mapM_ put =<< cards
    ("pick":_)     -> do
              (opts, ids) <- parseOpt allOptions $ tail args

              if optAll opts
                then do
                  dueCards <- filterM isDue =<< cards

                  forM_ dueCards $
                            \ card -> do
                              isOk <- ask card

                              (=<<) (update card)
                                      (if isOk
                                         then promote card
                                         else demote card)
                else do
                  mFirst <- first =<< cards
                  let ids' = map read ids
                  mCards <- mapM get ids'
                  let mCards' = if null mCards then [mFirst] else mCards

                  forM_ (catMaybes mCards') $
                            \ card -> do
                              isOk <- ask card

                              (=<<) (update card)
                                      (if isOk
                                         then promote card
                                         else demote card)
    ("remove":ids) -> if null ids
                        then delete =<< liftM fromJust maxId
                        else mapM_ (delete . read) ids
    _              -> putStrLn ("Argument has to be either add, " ++
                                "edit [ids...], export, info, list, " ++
                                "pick [ids...|-a|--all] or remove [ids...].")
