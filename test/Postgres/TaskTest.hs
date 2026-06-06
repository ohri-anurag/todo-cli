module Postgres.TaskTest where

import Data.Time (getCurrentTimeZone)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Session qualified
import NonEmptyText (NonEmptyText (..))
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Init qualified as Postgres.Init
import Postgres.Task (listNonCompletedTasks, listTasks, unpackAll)
import Postgres.Task.Complete (completeTasks)
import Postgres.Task.Complete qualified as Postgres.Complete
import Postgres.Task.Insert (TaskOptions (..), insertTaskQuery)
import Postgres.Task.Insert qualified as Postgres.Insert
import Postgres.Task.Update (Updates (..), updateTaskQuery)
import Rel8 (showInsert, showQuery, showUpdate)
import Relude
import Repeat (Repeat (..))
import Setup qualified
import Task qualified
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_insertTask :: TestTree
test_insertTask =
  goldenVsString "insertTask" "test/golden/insertTask.golden.txt"
    $ pure
    . encodeUtf8
    . showInsert
    . insertTaskQuery (Schema (NonEmptyText 'p' "ublic")) (TableName (NonEmptyText 't' "asks"))
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
    $ completeTasks
      (Schema (NonEmptyText 'p' "ublic"))
      (TableName (NonEmptyText 't' "asks"))
      [2]

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
    $ updateTaskQuery
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

test_completeFlowRepeat :: TestTree
test_completeFlowRepeat = goldenVsString "completeFlowRepeat" "test/golden/completeFlowRepeat.golden.txt" $ do
  let schema = Schema (NonEmptyText 't' "est_schema")
      table = TableName (NonEmptyText 't' "asks")
  envStr <- lookupEnv "PG_CONN_STRING"
  let connStr = fromMaybe "postgresql://localhost:5432/postgres" $ toText <$> envStr
      connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string connStr

  result <- runExceptT $ do
    conn <- ExceptT $ first show <$> Hasql.Connection.acquire [connSetting]
    bytes <- ExceptT $ fmap (first show) $ flip Hasql.Session.run conn $ do
      Postgres.Init.initTaskTable schema table

      Postgres.Insert.addTask schema table
        $ Postgres.Insert.TaskOptions
          { description = Identity (NonEmptyText 'G' "randparent"),
            due = Nothing,
            parent = Nothing,
            remindAt = Nothing,
            repeatAfter = Just Daily,
            tags = Nothing
          }

      Postgres.Insert.addTask schema table
        $ Postgres.Insert.TaskOptions
          { description = Identity (NonEmptyText 'p' "arent"),
            due = Nothing,
            parent = Just 1,
            remindAt = Nothing,
            repeatAfter = Just Weekly,
            tags = Nothing
          }

      Postgres.Insert.addTask schema table
        $ Postgres.Insert.TaskOptions
          { description = Identity (NonEmptyText 'G' "randparent"),
            due = Nothing,
            parent = Just 2,
            remindAt = Nothing,
            repeatAfter = Just Daily,
            tags = Nothing
          }

      let displayTasks = do
            tasks <- listTasks schema table
            tz <- liftIO getCurrentTimeZone
            pure
              $ encodeUtf8
              $ mconcat
              $ fmap (Task.display tz Setup.defaultPalette)
              $ unpackAll tasks

      Postgres.Complete.completeTask schema table 1
      d1 <- displayTasks

      Postgres.Complete.completeTask schema table 5
      d2 <- displayTasks

      Postgres.Complete.completeTask schema table 8
      d3 <- displayTasks

      Hasql.Session.sql "drop schema if exists test_schema cascade"
      pure $ mconcat [d1, d2, d3]

    lift $ Hasql.Connection.release conn
    pure bytes

  pure $ case result of
    Left err -> "Test failed: " <> err
    Right bytes -> bytes
