module Postgres.TaskTest where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText (NonEmptyText (..))
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (completeTasks, listNonCompletedTasks)
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
    . insertTask (Schema (NonEmptyText 'p' "ublic")) (TableName (NonEmptyText 't' "asks"))
    $ TaskOptions
      { description = Identity (NonEmptyText 'T' "his is a test"),
        due = Just $ posixSecondsToUTCTime 1779453522,
        parent = Just 2,
        remindAt = Just $ posixSecondsToUTCTime 1779451111,
        repeatAfter = Just Repeat.Daily,
        tags =
          Just
            $ (NonEmptyText 's' "imple")
            :| [(NonEmptyText 't' "est")]
      }

test_completeTask :: TestTree
test_completeTask =
  goldenVsString "completeTasks" "test/golden/completeTasks.golden.txt"
    $ pure
    . encodeUtf8
    . showUpdate
    $ completeTasks (Schema (NonEmptyText 'p' "ublic")) (TableName (NonEmptyText 't' "asks")) [2]

test_listNonCompletedTasks :: TestTree
test_listNonCompletedTasks =
  goldenVsString "listNonCompletedTasks" "test/golden/listNonCompletedTasks.golden.txt"
    $ pure
    . encodeUtf8
    . showQuery
    $ listNonCompletedTasks (Schema (NonEmptyText 'p' "ublic")) (TableName (NonEmptyText 't' "asks"))

test_updateTask :: TestTree
test_updateTask =
  goldenVsString "updateTask" "test/golden/updateTask.golden.txt"
    $ pure
    . encodeUtf8
    . showUpdate
    $ updateTask
      (Schema (NonEmptyText 'p' "ublic"))
      (TableName (NonEmptyText 't' "asks"))
      2
      ( UpdateDescription (NonEmptyText 'T' "est")
          :| [ UpdateDue (posixSecondsToUTCTime 1779453522),
               UpdateParent 2,
               UpdateRemindAt (posixSecondsToUTCTime 1779451111),
               UpdateRepeatAfter Repeat.Daily,
               UpdateTags
                 ( (NonEmptyText 's' "imple")
                     :| [(NonEmptyText 't' "est")]
                 )
             ]
      )
