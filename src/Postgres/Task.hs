{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Postgres.Task where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Hasql.Session qualified
import Hasql.Transaction qualified
import Hasql.Transaction.Sessions qualified
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Rel8
  ( Column,
    Expr,
    Name,
    QualifiedName (..),
    Query,
    Rel8able,
    Result,
    TableSchema (..),
    each,
    filter,
    not_,
    run,
    select,
  )
import Relude hiding (filter, id, repeat)
import Relude qualified as Relude (filter)
import Repeat (Repeat (..))
import Task qualified

data Task f = Task
  { createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime,
    id :: Column f Int64,
    isCompleted :: Column f Bool,
    description :: Column f NonEmptyText,
    due :: Column f (Maybe UTCTime),
    remindAt :: Column f (Maybe UTCTime),
    repeatAfter :: Column f (Maybe Repeat),
    parent :: Column f (Maybe Int64),
    tags :: Column f (Maybe NonEmptyText)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving instance Show (Task Result)

-- | NOTE: This function assumes that there will always be
-- root level task in the provided list of tasks.
--
-- It is fine to use when fetching all tasks from the DB.
-- Use with caution otherwise.
unpackAll :: [Task Result] -> [Task.Task]
unpackAll postgresTasks =
  let idMap = Map.fromList $ postgresTasks <&> \(task@Task {id}) -> (id, task)
      childMap =
        Map.unionsWith (<>)
          $ mapMaybe
            ( \(child@Task {parent}) -> do
                parentId <- parent
                Task {id} <- Map.lookup parentId idMap
                Just $ Map.singleton id $ child :| []
            )
            postgresTasks
   in fmap (unpack childMap) $ Relude.filter (\(Task {parent}) -> isNothing parent) postgresTasks

unpack :: Map Int64 (NonEmpty (Task Result)) -> Task Result -> Task.Task
unpack childMap Task {..} =
  Task.Task
    { description = description,
      due = due,
      id = id,
      remindAt = remindAt,
      repeatAfter = repeatAfter,
      subTasks = unpack childMap <<$>> Map.lookup id childMap,
      tags = do
        listNeTags <-
          mapMaybe (rightToMaybe . NonEmptyText.parse)
            . Text.splitOn ","
            . toText
            <$> tags
        nonEmpty listNeTags
    }

taskSchema :: Schema -> TableName -> TableSchema (Task Name)
taskSchema (Schema schema) (TableName table) =
  TableSchema
    { name =
        QualifiedName
          { name = toString table,
            schema = Just $ toString schema
          },
      columns =
        Task
          { createdAt = "created_at",
            updatedAt = "updated_at",
            id = "id",
            isCompleted = "is_completed",
            description = "description",
            due = "due",
            remindAt = "remind_at",
            repeatAfter = "repeat_after",
            parent = "parent",
            tags = "tags"
          }
    }

listNonCompletedTasks :: Schema -> TableName -> Query (Task Expr)
listNonCompletedTasks schema table = do
  task <- each $ taskSchema schema table
  filter (\(Task {isCompleted}) -> not_ $ isCompleted) task

listTasks :: Schema -> TableName -> Hasql.Session.Session [Task Result]
listTasks schema table =
  Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
    $ Hasql.Transaction.statement ()
    $ run
    $ select
    $ listNonCompletedTasks schema table
