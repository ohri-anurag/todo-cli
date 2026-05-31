module Task where

import Data.Time (TimeZone, UTCTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import NonEmptyText (NonEmptyText)
import Relude hiding (repeat)
import Repeat (Repeat)

data Task
  = TaskWithoutSubTasks (Task' Proxy)
  | TaskWithSubTasks (Task' Identity)
  deriving stock (Show, Generic)

type TaskWithoutSubTasks = Task' Proxy

type TaskWithSubTasks = Task' Identity

data Task' f = Task
  { description :: NonEmptyText,
    due :: Maybe UTCTime,
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

display :: TimeZone -> Task -> Text
display tz = \case
  TaskWithSubTasks Task {..} ->
    unlines
      $ catMaybes
        [ Just "======== TASK BEGINS ========",
          Just $ "Description: " <> toText description,
          formatUTCTime tz "Due: " <$> due,
          formatUTCTime tz "Remind at: " <$> remindAt,
          repeatAfter <&> \r -> "Repeats: " <> show r,
          tags <&> \t -> fold $ "Tags: " : intersperse ", " (fmap (\tag -> "#" <> toText tag) $ toList t)
        ]
  TaskWithoutSubTasks Task {..} ->
    unlines
      $ catMaybes
        [ Just "======== TASK BEGINS ========",
          Just $ "Description: " <> toText description,
          formatUTCTime tz "Due: " <$> due,
          formatUTCTime tz "Remind at: " <$> remindAt,
          repeatAfter <&> \r -> "Repeats: " <> show r,
          tags <&> \t -> fold $ "Tags: " : intersperse ", " (fmap (\tag -> "#" <> toText tag) $ toList t)
        ]
