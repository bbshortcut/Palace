module Main where

import CardDB (cards, delete, first, get, insert, maxId, nextId, putDBInfo,
               setDBUp, update)
import Cards (ask, create, demote, edit, export, isDue, promote, put)
import Control.Monad (forM_, liftM, when)
import Data.Maybe (catMaybes, fromJust)
import System.Environment (getArgs)

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
    ("pick":ids)   -> do
              mFirst <- first =<< cards
              let ids' = map read ids
              mCards <- mapM get ids'

              let mCards' = if null mCards then [mFirst] else mCards

              forM_ (catMaybes mCards') $
                        \ card -> do
                          isDue <- isDue card

                          when isDue $ do
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
                                "pick [ids...] or remove [ids...].")
