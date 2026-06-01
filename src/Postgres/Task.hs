{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Postgres.Task where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import NonEmptyText (NonEmptyText (..))
import Postgres.Details (Schema (..), TableName (..))
import Refined (refineFail)
import Rel8
  ( Column,
    Expr,
    Name,
    QualifiedName (..),
    Query,
    Rel8able,
    Result,
    Returning (NoReturning),
    TableSchema (..),
    Update (..),
    each,
    filter,
    lit,
    not_,
    (==.),
  )
import Rel8.Expr.Time (now)
import Relude hiding (filter, id)
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
  let childrenMaybe = Map.lookup id childMap
   in case childrenMaybe of
        Nothing ->
          Task.TaskWithoutSubTasks
            Task.Task
              { description = description,
                due = due,
                id = id,
                remindAt = remindAt,
                repeatAfter = repeatAfter,
                subTasks = Proxy,
                tags = do
                  listNeTags <-
                    mapMaybe (fmap NonEmptyText . refineFail)
                      . Text.splitOn ","
                      . toText
                      <$> tags
                  nonEmpty listNeTags
              }
        Just children ->
          Task.TaskWithSubTasks
            Task.Task
              { description = description,
                due = due,
                id = id,
                remindAt = remindAt,
                repeatAfter = repeatAfter,
                subTasks = Identity $ fmap (unpack childMap) children,
                tags = do
                  listNeTags <-
                    mapMaybe (fmap NonEmptyText . refineFail)
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

completeTask :: Schema -> TableName -> Int64 -> Update ()
completeTask schema table searchId =
  Update
    { target = taskSchema schema table,
      from = pure (),
      set = \_from row -> row {isCompleted = lit True, updatedAt = now},
      updateWhere = \_from Task {id} -> id ==. lit searchId,
      returning = NoReturning
    }

listNonCompletedTasks :: Schema -> TableName -> Query (Task Expr)
listNonCompletedTasks schema table = do
  task <- each $ taskSchema schema table
  filter (\(Task {isCompleted}) -> not_ $ isCompleted) task
