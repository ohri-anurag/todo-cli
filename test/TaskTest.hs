module TaskTest where

import Data.Aeson qualified as Aeson
import Relude
import Task (TaskWithSubTasks, TaskWithoutSubTasks)
import Test.Tasty.Golden.Extra.GoldenVsToJSON (GoldenVsToJSON (..))

tasty_fullTask :: GoldenVsToJSON
tasty_fullTask =
  GoldenVsToJSON "test/golden/task-full.golden.json"
    $ Aeson.eitherDecodeFileStrict @TaskWithSubTasks "test/golden/task-full.json"

tasty_minimalTask :: GoldenVsToJSON
tasty_minimalTask =
  GoldenVsToJSON "test/golden/task-minimal.golden.json"
    $ Aeson.eitherDecodeFileStrict @TaskWithoutSubTasks "test/golden/task-minimal.json"
