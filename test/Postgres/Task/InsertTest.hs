module Postgres.Task.InsertTest where

import Data.Aeson qualified as Aeson
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText (NonEmptyText (..))
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task.Insert (AddTaskOptions (..), UpdateTaskOptions (..), insertTaskQuery)
import Postgres.Task.Insert qualified as Postgres.Insert
import Rel8 (showInsert)
import Relude
import Repeat (Repeat (..))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Extra.GoldenVsToJSON (GoldenVsToJSON (..))

test_insertTask :: TestTree
test_insertTask =
  goldenVsString "insertTask" "test/golden/insertTask.golden.txt"
    $ pure
    . encodeUtf8
    . showInsert
    $ insertTaskQuery
      (Schema (NonEmptyText 'p' "ublic"))
      (TableName (NonEmptyText 't' "asks"))
    $ Postgres.Insert.AddTaskOptions
      ( Postgres.Insert.Options
          { due = Just $ posixSecondsToUTCTime 1779453522,
            parent = Just 2,
            remindAt = Just $ posixSecondsToUTCTime 1779451111,
            repeatAfter = Just Repeat.Daily,
            tags =
              Just
                $ (NonEmptyText 's' "imple")
                :| [(NonEmptyText 't' "est")]
          }
      )
      (NonEmptyText 'T' "his is a test")

tasty_addTaskOptions :: GoldenVsToJSON
tasty_addTaskOptions =
  GoldenVsToJSON ("test/golden/AddTaskOptions.golden.json")
    $ Aeson.eitherDecodeFileStrict @AddTaskOptions ("test/golden/AddTaskOptions.json")

tasty_updateTaskOptions :: GoldenVsToJSON
tasty_updateTaskOptions =
  GoldenVsToJSON ("test/golden/UpdateTaskOptions.golden.json")
    $ Aeson.eitherDecodeFileStrict @UpdateTaskOptions ("test/golden/UpdateTaskOptions.json")
