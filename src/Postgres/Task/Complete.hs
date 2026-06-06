module Postgres.Task.Complete where

import Hasql.Session qualified
import Hasql.Transaction qualified
import Hasql.Transaction.Sessions qualified
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (Task (..), taskSchema)
import Rel8
  ( Expr,
    Insert (..),
    OnConflict (DoNothing),
    Query,
    Result,
    Returning (..),
    Update (..),
    each,
    filter,
    in_,
    insert,
    isNull,
    lit,
    loop,
    nullify,
    run,
    run_,
    select,
    unsafeDefault,
    update,
    values,
    (&&.),
    (==.),
    (||.),
  )
import Rel8.Expr.Time (now)
import Relude hiding (filter, id, repeat)
import Repeat (Repeat, getRepeatedTime)

completeTask :: Schema -> TableName -> Int64 -> Hasql.Session.Session ()
completeTask schema table index =
  Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write $ do
    taskAndAllSubTasks <-
      Hasql.Transaction.statement ()
        . run
        . select
        $ fetchTaskAndSubtasksQuery schema table index
    let oldIds = taskAndAllSubTasks <&> \Task {id} -> id
    Hasql.Transaction.statement ()
      . run_
      . update
      $ completeTasks schema table oldIds

    let repeatMaybe = do
          Task {repeatAfter} <- find (\Task {id} -> id == index) taskAndAllSubTasks
          repeatAfter
    for_ repeatMaybe $ \repeat -> do
      ids <-
        Hasql.Transaction.statement ()
          . run
          . insert
          $ cloneTasks schema table repeat taskAndAllSubTasks
      let idPairs = zip oldIds ids
      Hasql.Transaction.statement ()
        . run_
        . update
        $ updateTasks schema table idPairs

fetchTaskAndSubtasksQuery :: Schema -> TableName -> Int64 -> Query (Task Expr)
fetchTaskAndSubtasksQuery schema table parentId = loop base recursive
  where
    base :: Query (Task Expr)
    base = do
      task <- each $ taskSchema schema table
      filter (\(Task {id}) -> id ==. lit parentId) task

    recursive :: Task Expr -> Query (Task Expr)
    recursive Task {id} = do
      task <- each $ taskSchema schema table
      filter
        ( \Task {parent, repeatAfter, isCompleted} ->
            parent
              ==. nullify id
              -- For repeatable tasks that are children tasks,
              -- we don't want to create clones of already completed ones
              -- because they are essentially the same.
              --
              -- So we check for non completed ones.
              &&. (isNull repeatAfter ||. isCompleted ==. lit False)
        )
        task

cloneTasks :: Schema -> TableName -> Repeat -> [Task Result] -> Insert (Query (Expr Int64))
cloneTasks schema table repeat tasks =
  Insert
    { into = taskSchema schema table,
      rows = values $ task' <$> tasks,
      onConflict = DoNothing,
      returning = Returning id
    }
  where
    task' Task {..} =
      Task
        { createdAt = now,
          updatedAt = now,
          id = unsafeDefault,
          isCompleted = lit False,
          description = lit description,
          due = lit $ getRepeatedTime repeat <$> due,
          remindAt = lit $ getRepeatedTime repeat <$> remindAt,
          repeatAfter = lit repeatAfter,
          parent = lit parent,
          tags = lit tags
        }

updateTasks :: Schema -> TableName -> [(Int64, Int64)] -> Update ()
updateTasks schema table idPairs =
  Update
    { target = taskSchema schema table,
      from = values $ lit . bimap Just Just <$> idPairs,
      set = \(_, newId) row -> row {parent = newId, updatedAt = now},
      updateWhere =
        \(oldId, _) Task {parent, isCompleted} ->
          parent ==. oldId &&. isCompleted ==. lit False,
      returning = NoReturning
    }

completeTasks :: Schema -> TableName -> [Int64] -> Update ()
completeTasks schema table ids =
  Update
    { target = taskSchema schema table,
      from = pure (),
      set = \_from row -> row {isCompleted = lit True, updatedAt = now},
      updateWhere = \_from Task {id} -> id `in_` fmap lit ids,
      returning = NoReturning
    }
