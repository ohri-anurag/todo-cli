module Postgres.Task.Update where

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
    Returning (NoReturning),
    Update (..),
    lit,
    run_,
    update,
    (==.),
  )
import Rel8.Expr.Time (now)
import Relude hiding (id)
import Repeat (Repeat (..))

data Updates
  = UpdateDescription NonEmptyText
  | UpdateDue UTCTime
  | UpdateParent Int64
  | UpdateRemindAt UTCTime
  | UpdateRepeatAfter Repeat.Repeat
  | UpdateTags (NonEmpty NonEmptyText)

updateTaskQuery :: Schema -> TableName -> Int64 -> NonEmpty Updates -> Update ()
updateTaskQuery schema table updateId updates =
  let updateRow :: Task Expr -> Updates -> Task Expr
      updateRow row = \case
        UpdateDescription description -> row {description = lit description}
        UpdateDue due -> row {due = lit $ Just due}
        UpdateParent parent -> row {parent = lit $ Just parent}
        UpdateRemindAt remindAt -> row {remindAt = lit $ Just remindAt}
        UpdateRepeatAfter repeatAfter -> row {repeatAfter = lit $ Just repeatAfter}
        UpdateTags tags -> row {tags = lit $ Just . fold1 . NonEmpty.intersperse (NonEmptyText ',' "") $ tags}
   in Update
        { target = taskSchema schema table,
          from = pure (),
          set = \_from row -> (foldl' updateRow row updates) {updatedAt = now},
          updateWhere = \_from Task {id} -> id ==. lit updateId,
          returning = NoReturning
        }

updateTask :: Schema -> TableName -> Int64 -> NonEmpty Updates -> Hasql.Session.Session ()
updateTask schema table index updates =
  Hasql.Transaction.Sessions.transaction Hasql.Transaction.Sessions.Serializable Hasql.Transaction.Sessions.Write
    $ Hasql.Transaction.statement ()
    $ run_
    $ update
    $ updateTaskQuery schema table index updates
