{-# LANGUAGE TemplateHaskell #-}

module Postgres.TaskTest where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (completeTask, listNonCompletedTasks)
import Postgres.Task.Insert (TaskOptions (..), insertTask)
import Postgres.Task.Update (Updates (..), updateTask)
import Rel8 (showInsert, showQuery, showUpdate)
import Relude
import Repeat qualified
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_insertTask :: TestTree
test_insertTask =
  goldenVsString "insertTask" "test/golden/insertTask.golden.txt"
    $ pure
    . encodeUtf8
    . showInsert
    . insertTask (Schema $$(NonEmptyText.make "public")) (TableName $$(NonEmptyText.make "tasks"))
    $ TaskOptions
      { description = Identity $$(NonEmptyText.make "This is a test"),
        due = Just $ posixSecondsToUTCTime 1779453522,
        remindAt = Just $ posixSecondsToUTCTime 1779451111,
        repeatAfter = Just Repeat.Daily,
        tags =
          Just
            $ $$(NonEmptyText.make "simple")
            :| [$$(NonEmptyText.make "test")]
      }

test_completeTask :: TestTree
test_completeTask =
  goldenVsString "completeTask" "test/golden/completeTask.golden.txt"
    $ pure
    . encodeUtf8
    . showUpdate
    $ completeTask (Schema $$(NonEmptyText.make "public")) (TableName $$(NonEmptyText.make "tasks")) 2

test_listNonCompletedTasks :: TestTree
test_listNonCompletedTasks =
  goldenVsString "listNonCompletedTasks" "test/golden/listNonCompletedTasks.golden.txt"
    $ pure
    . encodeUtf8
    . showQuery
    $ listNonCompletedTasks (Schema $$(NonEmptyText.make "public")) (TableName $$(NonEmptyText.make "tasks"))

test_updateTask :: TestTree
test_updateTask =
  goldenVsString "updateTask" "test/golden/updateTask.golden.txt"
    $ pure
    . encodeUtf8
    . showUpdate
    $ updateTask
      (Schema $$(NonEmptyText.make "public"))
      (TableName $$(NonEmptyText.make "tasks"))
      2
      ( UpdateDescription $$(NonEmptyText.make "Test")
          :| [ UpdateDue (posixSecondsToUTCTime 1779453522),
               UpdateRemindAt (posixSecondsToUTCTime 1779451111),
               UpdateRepeatAfter Repeat.Daily,
               UpdateTags
                 ( $$(NonEmptyText.make "simple")
                     :| [$$(NonEmptyText.make "test")]
                 )
             ]
      )
