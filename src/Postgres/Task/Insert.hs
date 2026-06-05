module Postgres.Task.Insert where

import Data.Foldable1 (fold1)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (UTCTime)
import Hasql.Session qualified
import Hasql.Transaction qualified
import Hasql.Transaction.Sessions qualified
import NonEmptyText (NonEmptyText (..))
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (Task (..), taskSchema)
import Rel8
  ( Expr,
    Insert (..),
    OnConflict (DoNothing),
    Returning (NoReturning),
    insert,
    lit,
    run_,
    unsafeDefault,
    values,
  )
import Rel8.Expr.Time (now)
import Relude
import Repeat (Repeat (..))

data TaskOptions f = TaskOptions
  { description :: f NonEmptyText,
    due :: Maybe UTCTime,
    parent :: Maybe Int64,
    remindAt :: Maybe UTCTime,
    repeatAfter :: Maybe Repeat,
    tags :: Maybe (NonEmpty NonEmptyText)
  }

type AddTaskOptions = TaskOptions Identity

type UpdateTaskOptions = TaskOptions Maybe

insertTaskQuery :: Schema -> TableName -> AddTaskOptions -> Insert ()
insertTaskQuery schema table TaskOptions {..} =
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
          isCompleted = lit False,
          description = lit $ runIdentity description,
          due = lit due,
          remindAt = lit remindAt,
          repeatAfter = lit repeatAfter,
          parent = lit parent,
          tags = lit $ (fold1 . NonEmpty.intersperse (NonEmptyText ',' "")) <$> tags
        }

addTask :: Schema -> TableName -> AddTaskOptions -> Hasql.Session.Session ()
addTask schema table taskOptions =
  Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
    $ Hasql.Transaction.statement ()
    $ run_
    $ insert
    $ insertTaskQuery schema table taskOptions
