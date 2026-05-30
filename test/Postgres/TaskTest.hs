{-# LANGUAGE TemplateHaskell #-}

module Postgres.TaskTest where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (insertTask)
import Rel8 (showInsert)
import Relude
import Task (Seconds (..), Task' (..))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

tasty_insertTask :: TestTree
tasty_insertTask =
  goldenVsString "insertTask" "test/golden/insertTask.golden.txt"
    $ pure
    . encodeUtf8
    . showInsert
    . insertTask (Schema $$(NonEmptyText.make "public")) (TableName $$(NonEmptyText.make "tasks"))
    $ Task
      { description = $$(NonEmptyText.make "This is a test"),
        due = Just $ posixSecondsToUTCTime 1779453522,
        remindAt = Just $ posixSecondsToUTCTime 1779451111,
        repeatAfter = Just $ Seconds 3600,
        subTasks = Proxy,
        tags =
          Just
            $ $$(NonEmptyText.make "simple")
            :| [$$(NonEmptyText.make "test")]
      }
