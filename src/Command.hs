module Command where

import Postgres.Task.Insert (AddTaskOptions (..), UpdateTaskOptions (..))
import Relude

data Command
  = AddTask AddTaskOptions
  | CompleteTask Int64
  | Init SetupMethod
  | List
  | Setup SetupMethod
  | UpdateTask Int64 UpdateTaskOptions
  | Version

data SetupMethod = Postgres
