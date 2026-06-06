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

runFlowTest :: Text -> (Schema -> TableName -> Hasql.Session.Session LByteString) -> IO LByteString
runFlowTest schemaName body = do
  let schema = Schema (NonEmptyText 't' schemaName)
      table = TableName (NonEmptyText 't' "asks")
  envStr <- lookupEnv "PG_CONN_STRING"
  let connStr = fromMaybe "postgresql://localhost:5432/postgres" $ toText <$> envStr
      connSetting = Hasql.Connection.Setting.connection $ Hasql.Connection.Setting.Connection.string connStr
  result <- runExceptT $ do
    conn <- ExceptT $ first show <$> Hasql.Connection.acquire [connSetting]
    bytes <- ExceptT $ fmap (first show) $ flip Hasql.Session.run conn $ do
      Postgres.Init.initTaskTable schema table
      output <- body schema table
      Hasql.Session.sql $ "drop schema if exists " <> encodeUtf8 schemaName <> " cascade"
      pure output
    lift $ Hasql.Connection.release conn
    pure bytes
  pure $ case result of
    Left err -> "Test failed: " <> err
    Right bytes -> bytes

displayTasks :: Schema -> TableName -> Hasql.Session.Session LByteString
displayTasks schema table = do
  tasks <- listTasks schema table
  tz <- liftIO getCurrentTimeZone
  pure $ encodeUtf8 $ mconcat $ fmap (Task.display tz Setup.defaultPalette) $ unpackAll tasks

insertTask :: Schema -> TableName -> NonEmptyText -> Maybe Int64 -> Maybe Repeat -> Hasql.Session.Session ()
insertTask schema table desc parent repeatAfter =
  Postgres.Insert.addTask schema table
    $ Postgres.Insert.TaskOptions
      { description = Identity desc,
        due = Nothing,
        parent = parent,
        remindAt = Nothing,
        repeatAfter = repeatAfter,
        tags = Nothing
      }

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
test_completeFlowRepeat = goldenVsString "completeFlowRepeat" "test/golden/completeFlowRepeat.golden.txt"
  $ runFlowTest "test_schema"
  $ \schema table -> do
    insertTask schema table (NonEmptyText 'G' "randparent") Nothing (Just Daily)
    insertTask schema table (NonEmptyText 'p' "arent") (Just 1) (Just Weekly)
    insertTask schema table (NonEmptyText 'G' "randparent") (Just 2) (Just Daily)

    Postgres.Complete.completeTask schema table 1
    d1 <- displayTasks schema table

    Postgres.Complete.completeTask schema table 5
    d2 <- displayTasks schema table

    Postgres.Complete.completeTask schema table 8
    d3 <- displayTasks schema table

    pure $ mconcat [d1, d2, d3]

test_completeFlowNonRepeatGrandparent :: TestTree
test_completeFlowNonRepeatGrandparent = goldenVsString "completeFlowNonRepeatGrandparent" "test/golden/completeFlowNonRepeatGrandparent.golden.txt"
  $ runFlowTest "test_schema_nr_gp"
  $ \schema table -> do
    insertTask schema table (NonEmptyText 'G' "randparent") Nothing Nothing
    insertTask schema table (NonEmptyText 'p' "arent") (Just 1) Nothing
    insertTask schema table (NonEmptyText 'c' "hild") (Just 2) Nothing

    Postgres.Complete.completeTask schema table 1
    displayTasks schema table

test_completeFlowNonRepeatParent :: TestTree
test_completeFlowNonRepeatParent = goldenVsString "completeFlowNonRepeatParent" "test/golden/completeFlowNonRepeatParent.golden.txt"
  $ runFlowTest "test_schema_nr_p"
  $ \schema table -> do
    insertTask schema table (NonEmptyText 'G' "randparent") Nothing Nothing
    insertTask schema table (NonEmptyText 'p' "arent") (Just 1) Nothing
    insertTask schema table (NonEmptyText 'c' "hild") (Just 2) Nothing

    Postgres.Complete.completeTask schema table 2
    displayTasks schema table

test_completeFlowNonRepeatChild :: TestTree
test_completeFlowNonRepeatChild = goldenVsString "completeFlowNonRepeatChild" "test/golden/completeFlowNonRepeatChild.golden.txt"
  $ runFlowTest "test_schema_nr_c"
  $ \schema table -> do
    insertTask schema table (NonEmptyText 'G' "randparent") Nothing Nothing
    insertTask schema table (NonEmptyText 'p' "arent") (Just 1) Nothing
    insertTask schema table (NonEmptyText 'c' "hild") (Just 2) Nothing

    Postgres.Complete.completeTask schema table 3
    displayTasks schema table
