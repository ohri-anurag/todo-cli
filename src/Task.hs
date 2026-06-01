module Task where

import Data.Text qualified as Text
import Data.Time (TimeZone, UTCTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import NonEmptyText (NonEmptyText)
import Relude hiding (id)
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

displayTask :: TimeZone -> Int -> Task' f -> Text
displayTask tz indent Task {..} =
  unlines
    $ catMaybes
    $ (Text.replicate indent " " <>)
    <<$>> [ Just $ "ID: " <> show id,
            Just $ "Description: " <> toText description,
            formatUTCTime tz "Due: " <$> due,
            formatUTCTime tz "Remind at: " <$> remindAt,
            repeatAfter <&> \r -> "Repeats: " <> show r,
            tags <&> \t -> fold $ "Tags: " : intersperse ", " (fmap (\tag -> "#" <> toText tag) $ toList t)
          ]

display :: TimeZone -> Task -> Text
display = helper 0
  where
    helper :: Int -> TimeZone -> Task -> Text
    helper indent tz = \case
      TaskWithSubTasks task@Task {subTasks} ->
        mconcat
          [ displayTask tz indent task,
            Text.replicate indent " ",
            "Sub-Tasks:\n",
            sconcat $ helper (indent + 2) tz <$> runIdentity subTasks
          ]
      TaskWithoutSubTasks task ->
        displayTask tz indent task
