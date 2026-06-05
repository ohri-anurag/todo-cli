module Task where

import Color qualified
import Data.Text qualified as Text
import Data.Time (TimeZone, UTCTime, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import NonEmptyText (NonEmptyText)
import Relude hiding (id, one)
import Repeat (Repeat)
import Setup qualified as Palette

data Task = Task
  { description :: NonEmptyText,
    due :: Maybe UTCTime,
    id :: Int64,
    remindAt :: Maybe UTCTime,
    repeatAfter :: Maybe Repeat,
    subTasks :: Maybe (NonEmpty Task),
    tags :: Maybe (NonEmpty NonEmptyText)
  }
  deriving stock (Show, Generic)

formatUTCTime :: TimeZone -> Text -> UTCTime -> Text
formatUTCTime tz prefix time =
  prefix <> toText (formatTime defaultTimeLocale "%A, %B %d, %Y at %R" $ utcToZonedTime tz time)

display :: TimeZone -> Palette.Palette -> Task -> Text
display tz palette task = helper 0 task <> "\n"
  where
    idColor = Color.colour $ Palette.id palette
    descriptionColor = Color.colour $ Palette.description palette
    dueColor = Color.colour $ Palette.due palette
    remindAtColor = Color.colour $ Palette.remindAt palette
    repeatColor = Color.colour $ Palette.repeat palette
    tagsColor = Color.colour $ Palette.tags palette
    subTasksColor = Color.colour $ Palette.subTasks palette

    helper :: Int -> Task -> Text
    helper indent Task {..} =
      Text.intercalate "\n"
        $ fmap (Text.replicate indent " " <>)
        $ catMaybes
          [ Just $ idColor $ "ID: " <> show id,
            Just $ descriptionColor $ "Description: " <> toText description,
            dueColor . formatUTCTime tz "Due: " <$> due,
            remindAtColor . formatUTCTime tz "Remind at: " <$> remindAt,
            repeatAfter <&> \r -> repeatColor $ "Repeats: " <> show r,
            tags <&> \t -> tagsColor . fold $ "Tags: " : intersperse ", " (fmap (\tag -> "#" <> toText tag) $ toList t),
            subTasks <&> \st -> ((subTasksColor "Sub-Tasks:" <> "\n") <>) . sconcat . fmap (helper (indent + 2)) $ st
          ]
