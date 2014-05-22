module Tasks where

type Name = String
type Content = [String]

data Status = Todo
            | InProgress
            | Pending
            | Delegated
            | Done
            | Canceled
            deriving (Eq, Show)

overStatuses :: [Status]
overStatuses = [Done, Canceled]

type Subtasks = [Task]

data Task =
  Task { _tName :: Name
       , _tContent :: Content
       , _tStatus :: Status
       , _tSubtasks :: Subtasks
       } deriving Show

task :: Name -> Content -> Task
task name content = Task name content Todo []

project :: Name -> Content -> Subtasks -> Task
project name content subtasks = Task name content Todo subtasks

changeStatus :: Task -> Status -> Task
changeStatus task status = task { _tStatus = status }

isOver :: Task -> Bool
isOver task = elem (_tStatus task) overStatuses
