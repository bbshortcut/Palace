module Utils ( FileName
             , Timestamp
             , addTrees
             , addTreeToForest
             , doesPathExist
             , getAnswer
             , getAnswerWithPrefill
             , getDirectoryContents'
             , getTimestamp
             , getTimestampsInDirectory
             , getYesOrNo
             , intersectTrees
             , itemPred
             , itemSucc
             , latestTimestamp
             , removeTrees
             , takeFileName'
             , timestampsOlderThan
             , today
             , unsort
             ) where

import Control.Monad (liftM)
import Data.Char (isDigit)
import Data.List (find, isPrefixOf, sortBy, stripPrefix)
import Data.Maybe (catMaybes, isJust, maybe)
import Data.Ord (comparing)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays)
import Data.Time.Clock (getCurrentTime, utctDay, utctDayTime, DiffTime)
import Data.Tree (Tree(..), Forest, levels, unfoldTree)
import System.Console.Readline (readline,
                                setPreInputHook, insertText, redisplay)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.FilePath (joinPath, splitDirectories, takeFileName)
import System.Random (Random, RandomGen, randoms)

itemPred :: Eq a => [a] -> a -> Maybe a
[] `itemPred` _ = Nothing
(_:[]) `itemPred` _ = Nothing
(x:y:zs) `itemPred` i
    | i == y    = Just x
    | otherwise = itemPred (y:zs) i

itemSucc :: Eq a => [a] -> a -> Maybe a
itemSucc xs = itemPred (reverse xs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sameRoot :: Eq a => Tree a -> Tree a -> Bool
sameRoot x y = rootLabel x == rootLabel y

addTrees :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
addTrees treeA treeB
    | sameRoot treeA treeB =
        Just $ Node (rootLabel treeA) $
             addForests (subForest treeA) (subForest treeB)
    | otherwise = Nothing

addForests :: Eq a => Forest a -> Forest a -> Forest a
addForests = foldl addTreeToForest

addTreeToForest :: Eq a => Forest a -> Tree a -> Forest a
addTreeToForest [] tree = [tree]
addTreeToForest (tree : trees) oneMoreTree =
    maybe (tree : addTreeToForest trees oneMoreTree) ((flip (:)) trees)
              (addTrees tree oneMoreTree)

removeTrees :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
removeTrees treeA treeB
    | sameRoot treeA treeB =
        let remainingSubForest =
                removeForests (subForest treeA) (subForest treeB)
        in if null remainingSubForest
             then Nothing
             else Just $ treeA { subForest = remainingSubForest }
    | otherwise = Just treeA

removeForests :: Eq a => Forest a -> Forest a -> Forest a
removeForests forestA forestB =
    catMaybes $ map (mayFindAndRemove forestB) forestA
        where mayFindAndRemove :: Eq a => Forest a -> Tree a -> Maybe (Tree a)
              mayFindAndRemove forest tree =
                  maybe (Just tree) (removeTrees tree) $
                        find (sameRoot tree) forest

intersectTrees :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
intersectTrees treeA treeB =
    if sameRoot treeA treeB
      then Just $ unfoldTree intersectSeed (treeA, treeB)
      else Nothing

intersectSeed :: Eq a => (Tree a, Tree a) -> (a, [(Tree a, Tree a)])
intersectSeed (treeA, treeB) =
    if (depth treeA == 1) || (depth treeB == 1)
      then (rootLabel treeA, [])
      else (rootLabel treeA,
            filter (uncurry sameRoot) $
                   subForest treeA `cartProd` subForest treeB)
        where depth :: Tree a -> Int
              depth = length . levels

takePrefix :: Eq a => [a] -> [a] -> [a]
takePrefix (x:xs) (y:ys)
    | x == y    = x : takePrefix xs ys
    | otherwise = []
takePrefix _ _ = []

takeParent :: FilePath -> FilePath -> Maybe FilePath
takeParent pathA pathB =
    case takePrefix (splitDirectories pathA) (splitDirectories pathB) of
      [] -> Nothing
      xs -> Just $ joinPath xs

dropParent :: Maybe FilePath -> FilePath -> Maybe FilePath
dropParent Nothing path = Just path
dropParent (Just parent) path =
    case stripPrefix (splitDirectories parent) (splitDirectories path) of
      Nothing -> Nothing
      Just xs -> Just $ joinPath xs

haveSameParent :: FilePath -> FilePath -> Bool
haveSameParent path = isJust . takeParent path

type FileName = String

getDirectoryContents' :: FilePath -> IO [FileName]
getDirectoryContents' =
  liftM (filter (\ x -> x /= "." && x /= "..")) . getDirectoryContents

takeFileName' :: FilePath -> FilePath
takeFileName' "/" = "/"
takeFileName' path = takeFileName path

doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
  file <- doesFileExist path
  directory <- doesDirectoryExist path

  return $ file || directory

today :: IO Day
today = liftM utctDay getCurrentTime

type Timestamp = FileName

getTimestamp :: IO Timestamp
getTimestamp = do
  date <- today
  diff <- now

  let (year, month, day) = toGregorian date
      time = floor diff
      hours = quot time 3600
      minutes = quot (time - hours * 3600) 60

  return $ myShow year ++ myShow month ++ myShow day ++
         myShow hours ++ myShow minutes
      where today :: IO Day
            today = liftM utctDay getCurrentTime

            now :: IO DiffTime
            now = liftM utctDayTime getCurrentTime

            myShow :: (Integral a, Show a) => a -> String
            myShow x
                | length (show x) == 1 = '0' : show x
                | otherwise = show x

isTimestamp :: FileName -> Bool
isTimestamp ts
    | all isDigit ts && length ts == 12 &&
      read ts >= 200000000000 = True
    | otherwise = False

diffTimestamps :: Timestamp -> Timestamp -> Integer
diffTimestamps t1 t2 =
    diffDays (fromTimestamp t1) (fromTimestamp t2)
        where fromTimestamp :: Timestamp -> Day
              fromTimestamp ts = fromGregorian year month day
                  where year :: Integer
                        year = read $ take 4 ts

                        month :: Int
                        month = read $ take 2 $ drop 4 ts

                        day :: Int
                        day = read $ take 2 $ drop 6 ts

latestTimestamp :: [Timestamp] -> Maybe Timestamp
latestTimestamp timestamps =
  case timestamps of
    []        -> Nothing
    otherwise -> Just $ maximum timestamps

timestampsOlderThan :: Integer -> Timestamp -> [Timestamp] -> [Timestamp]
timestampsOlderThan lifespan latest = filter (latest `isYoungerThan`)
    where isYoungerThan :: Timestamp -> Timestamp -> Bool
          isYoungerThan tsA tsB = abs (diffTimestamps tsA tsB) > lifespan

getTimestampsInDirectory :: FilePath -> IO [Timestamp]
getTimestampsInDirectory path = do
  pathExists <- doesDirectoryExist path

  if pathExists
    then do
      contents <- getDirectoryContents path

      return $ filter isTimestamp contents
    else return []

getAnswerWithPrefill :: String -> String -> IO String
getAnswerWithPrefill prefill question = do
  setPreInputHook (if null prefill
                     then Nothing
                     else Just (do insertText prefill; redisplay))

  mAnswer <- readline (question ++ "?  ")

  case mAnswer of
    Nothing     -> error "Interrupted."
    Just answer -> return answer

getAnswer :: String -> IO String
getAnswer = getAnswerWithPrefill ""

getYesOrNo :: String -> IO Bool
getYesOrNo question =
    liftM (isPrefixOf "y") $ getAnswer (question ++ " (y/N)")

unsort :: RandomGen a => a -> [b] -> [b]
unsort g es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = randoms g :: [Integer]
