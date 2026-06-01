module Task where

import Color qualified
import Data.Text qualified as Text
import Data.Time (TimeZone, UTCTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import NonEmptyText (NonEmptyText)
import Relude hiding (id, one)
import Repeat (Repeat)
import Setup qualified as Palette

data Task
  = TaskWithoutSubTasks (Task' Proxy)
  | TaskWithSubTasks (Task' Identity)
  deriving stock (Show, Generic)

type TaskWithoutSubTasks = Task' Proxy

type TaskWithSubTasks = Task' Identity

data Task' f = Task
  { description :: NonEmptyText,
    due :: Maybe UTCTime,
    id :: Int64,
    remindAt :: Maybe UTCTime,
    repeatAfter :: Maybe Repeat,
    subTasks :: f (NonEmpty Task),
    tags :: Maybe (NonEmpty NonEmptyText)
  }
  deriving stock (Generic)

deriving instance Show TaskWithSubTasks

deriving instance Show TaskWithoutSubTasks

formatUTCTime :: TimeZone -> Text -> UTCTime -> Text
formatUTCTime tz prefix time =
  prefix <> toText (formatTime defaultTimeLocale "%A, %B %d, %Y at %R" $ utcToZonedTime tz time)

displayTask :: TimeZone -> Palette.Palette -> Int -> Task' f -> Text
displayTask tz palette indent Task {..} =
  let idColor = Color.colour $ Palette.id palette
      descriptionColor = Color.colour $ Palette.description palette
      dueColor = Color.colour $ Palette.due palette
      remindAtColor = Color.colour $ Palette.remindAt palette
      repeatColor = Color.colour $ Palette.repeat palette
      tagsColor = Color.colour $ Palette.tags palette
   in unlines
        $ catMaybes
        $ (Text.replicate indent " " <>)
        <<$>> [ Just $ idColor $ "ID: " <> show id,
                Just $ descriptionColor $ "Description: " <> toText description,
                dueColor . formatUTCTime tz "Due: " <$> due,
                remindAtColor . formatUTCTime tz "Remind at: " <$> remindAt,
                repeatAfter <&> \r -> repeatColor $ "Repeats: " <> show r,
                tags <&> \t -> tagsColor . fold $ "Tags: " : intersperse ", " (fmap (\tag -> "#" <> toText tag) $ toList t)
              ]

display :: TimeZone -> Palette.Palette -> Task -> Text
display tz palette = helper 0
  where
    subTasksColor = Color.colour $ Palette.subTasks palette

    helper :: Int -> Task -> Text
    helper indent = \case
      TaskWithSubTasks task@Task {subTasks} ->
        mconcat
          [ displayTask tz palette indent task,
            Text.replicate indent " ",
            subTasksColor
              $ "Sub-Tasks:\n"
              <> sconcat (helper (indent + 2) <$> runIdentity subTasks)
          ]
      TaskWithoutSubTasks task ->
        displayTask tz palette indent task
