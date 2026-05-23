{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Postgres.Task where

import Data.Foldable1 (fold1)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (UTCTime)
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Rel8
  ( Column,
    Expr,
    Insert (..),
    Name,
    OnConflict (DoNothing),
    QualifiedName (..),
    Query,
    Rel8able,
    Result,
    Returning (NoReturning),
    TableSchema (..),
    each,
    filter,
    lit,
    not_,
    unsafeDefault,
    values,
  )
import Rel8.Expr.Time (now)
import Relude hiding (filter)
import Task qualified

data Task f = Task
  { createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime,
    id :: Column f Int64,
    isSubTask :: Column f Bool,
    isCompleted :: Column f Bool,
    description :: Column f NonEmptyText,
    due :: Column f (Maybe UTCTime),
    remindAt :: Column f (Maybe UTCTime),
    repeatAfter :: Column f (Maybe Int64),
    subTasks :: Column f (Maybe NonEmptyText),
    tags :: Column f (Maybe NonEmptyText)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving instance Show (Task Result)

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
            isSubTask = "is_sub_task",
            isCompleted = "is_completed",
            description = "description",
            due = "due",
            remindAt = "remind_at",
            repeatAfter = "repeat_after",
            subTasks = "sub_tasks",
            tags = "tags"
          }
    }

insertTask :: Schema -> TableName -> Task.TaskWithoutSubTasks -> Insert ()
insertTask schema table Task.Task {..} =
  Insert
    { into = taskSchema schema table,
      rows = values [task'],
      onConflict = DoNothing,
      returning = NoReturning
    }
  where
    task' :: Task Expr
    task' =
      Task
        { createdAt = now,
          updatedAt = now,
          id = unsafeDefault,
          isSubTask = lit False,
          isCompleted = lit False,
          description = lit description,
          due = lit due,
          remindAt = lit remindAt,
          repeatAfter = lit $ (fromIntegral . (\(Task.Seconds s) -> s)) <$> repeatAfter,
          subTasks = lit Nothing,
          tags = lit $ (fold1 . NonEmpty.intersperse $$(NonEmptyText.make ",")) <$> tags
        }

listNonCompletedTasks :: Schema -> TableName -> Query (Task Expr)
listNonCompletedTasks schema table = do
  task <- each $ taskSchema schema table
  filter (\(Task {isCompleted}) -> not_ $ isCompleted) task
