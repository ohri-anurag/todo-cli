{-# LANGUAGE TemplateHaskell #-}

module Postgres.Task.Update where

import Data.Foldable1 (fold1)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (UTCTime)
import NonEmptyText (NonEmptyText (..))
import NonEmptyText qualified
import Postgres.Details (Schema (..), TableName (..))
import Postgres.Task (Task (..), taskSchema)
import Rel8
  ( Expr,
    Returning (NoReturning),
    Update (..),
    lit,
    (==.),
  )
import Relude hiding (id)
import Repeat (Repeat (..))

data Updates
  = UpdateDescription NonEmptyText
  | UpdateDue UTCTime
  | UpdateRemindAt UTCTime
  | UpdateRepeatAfter Repeat.Repeat
  | UpdateTags (NonEmpty NonEmptyText)

updateTask :: Schema -> TableName -> Int64 -> NonEmpty Updates -> Update ()
updateTask schema table updateId updates =
  let update :: Task Expr -> Updates -> Task Expr
      update row = \case
        UpdateDescription description -> row {description = lit description}
        UpdateDue due -> row {due = lit $ Just due}
        UpdateRemindAt remindAt -> row {remindAt = lit $ Just remindAt}
        UpdateRepeatAfter repeatAfter -> row {repeatAfter = lit $ Just repeatAfter}
        UpdateTags tags -> row {tags = lit $ Just . fold1 . NonEmpty.intersperse $$(NonEmptyText.make ",") $ tags}
   in Update
        { target = taskSchema schema table,
          from = pure (),
          set = \_from row -> foldl' update row updates,
          updateWhere = \_from Task {id} -> id ==. lit updateId,
          returning = NoReturning
        }
