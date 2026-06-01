{-# LANGUAGE TemplateHaskell #-}

module Postgres.Task.Insert where

import Data.Foldable1 (fold1)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (UTCTime)
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (Task (..), taskSchema)
import Rel8
  ( Expr,
    Insert (..),
    OnConflict (DoNothing),
    Returning (NoReturning),
    lit,
    unsafeDefault,
    values,
  )
import Rel8.Expr.Time (now)
import Relude
import Repeat (Repeat (..))

data TaskOptions f = TaskOptions
  { description :: f NonEmptyText,
    due :: Maybe UTCTime,
    remindAt :: Maybe UTCTime,
    repeatAfter :: Maybe Repeat,
    tags :: Maybe (NonEmpty NonEmptyText)
  }

type AddTaskOptions = TaskOptions Identity

type UpdateTaskOptions = TaskOptions Maybe

insertTask :: Schema -> TableName -> AddTaskOptions -> Insert ()
insertTask schema table TaskOptions {..} =
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
          parent = lit Nothing,
          tags = lit $ (fold1 . NonEmpty.intersperse $$(NonEmptyText.make ",")) <$> tags
        }
