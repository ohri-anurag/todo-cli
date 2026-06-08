{-# LANGUAGE DeriveAnyClass #-}

module Command where

import Data.Aeson (FromJSON, ToJSON)
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
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data SetupMethod = Postgres
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
