module Repeat where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..), UTCTime (..))
import Data.Time.Calendar
  ( addDays,
    addGregorianMonthsRollOver,
    addGregorianYearsRollOver,
    dayOfWeek,
    dayOfWeekDiff,
  )
import Refined (GreaterThan, Refined, refineFail, unrefine)
import Rel8 qualified
import Relude hiding (repeat, show, words)
import Prelude (Show (..))

data RepeatTime
  = Days
  | Weeks
  | Months
  | Years
  deriving (Eq)

instance Show RepeatTime where
  show = \case
    Days -> "days"
    Weeks -> "weeks"
    Months -> "months"
    Years -> "years"

data CustomWeek = CustomWeek
  { mon :: Bool,
    tue :: Bool,
    wed :: Bool,
    thu :: Bool,
    fri :: Bool,
    sat :: Bool,
    sun :: Bool
  }
  deriving (Eq)

instance Show CustomWeek where
  show CustomWeek {..} =
    fold
      $ intersperse ","
      $ catMaybes
        [ if mon then Just "Mon" else Nothing,
          if tue then Just "Tue" else Nothing,
          if wed then Just "Wed" else Nothing,
          if thu then Just "Thu" else Nothing,
          if fri then Just "Fri" else Nothing,
          if sat then Just "Sat" else Nothing,
          if sun then Just "Sun" else Nothing
        ]

data Repeat
  = Daily
  | Weekly
  | Monthly
  | Yearly
  | Custom (Refined (GreaterThan 1) Int) RepeatTime
  | CustomWeekly CustomWeek
  deriving (Eq)

getRepeatedTime :: Repeat -> UTCTime -> UTCTime
getRepeatedTime repeat time@UTCTime {utctDay} =
  case repeat of
    Daily -> time {utctDay = succ utctDay}
    Weekly -> time {utctDay = addDays 7 utctDay}
    Monthly -> time {utctDay = addGregorianMonthsRollOver 1 utctDay}
    Yearly -> time {utctDay = addGregorianYearsRollOver 1 utctDay}
    Custom timesRefined repeatTime ->
      let times = fromIntegral $ unrefine timesRefined
       in case repeatTime of
            Days -> time {utctDay = addDays times utctDay}
            Weeks -> time {utctDay = addDays (times * 7) utctDay}
            Months -> time {utctDay = addGregorianMonthsRollOver times utctDay}
            Years -> time {utctDay = addGregorianYearsRollOver times utctDay}
    CustomWeekly CustomWeek {..} ->
      let utctDayOfWeek = dayOfWeek utctDay
          daysOfCustomWeek :: [DayOfWeek]
          daysOfCustomWeek =
            drop 1
              $ dropWhile (utctDayOfWeek /=)
              $ cycle
              $ catMaybes
                [ if mon then Just Monday else Nothing,
                  if tue then Just Tuesday else Nothing,
                  if wed then Just Wednesday else Nothing,
                  if thu then Just Thursday else Nothing,
                  if fri then Just Friday else Nothing,
                  if sat then Just Saturday else Nothing,
                  if sun then Just Sunday else Nothing
                ]
       in case daysOfCustomWeek of
            [] -> error "Impossible!!!"
            d : _ ->
              time
                { utctDay =
                    addDays (fromIntegral $ dayOfWeekDiff d utctDayOfWeek) utctDay
                }

instance Show Repeat where
  show = \case
    Daily -> "daily"
    Weekly -> "weekly"
    Monthly -> "monthly"
    Yearly -> "yearly"
    Custom times repeatTime -> mconcat [show $ unrefine times, " ", show repeatTime]
    CustomWeekly customWeek -> show customWeek

parse :: String -> Maybe Repeat
parse = \case
  "daily" -> Just Daily
  "weekly" -> Just Weekly
  "monthly" -> Just Monthly
  "yearly" -> Just Yearly
  str ->
    let text = toText str
        days = filter (not . Text.null) $ Text.splitOn "," $ Text.toLower text
        words = Text.words text
     in if length days > 1
          then
            Just
              $ CustomWeekly
                CustomWeek
                  { mon = "mon" `elem` days,
                    tue = "tue" `elem` days,
                    wed = "wed" `elem` days,
                    thu = "thu" `elem` days,
                    fri = "fri" `elem` days,
                    sat = "sat" `elem` days,
                    sun = "sun" `elem` days
                  }
          else case words of
            [timesText, repeatTimeText] -> do
              times <- readMaybe $ toString timesText
              times' <- refineFail times
              repeatTime <- case repeatTimeText of
                "days" -> Just Days
                "weeks" -> Just Weeks
                "months" -> Just Months
                "years" -> Just Years
                _ -> Nothing
              Just $ Custom times' repeatTime
            _ -> Nothing

instance ToJSON Repeat where
  toJSON = String . toText . show

instance FromJSON Repeat where
  parseJSON =
    withText "Repeat"
      $ maybe (fail "Could not parse repeat!") pure
      . parse
      . toString

instance Rel8.DBType Repeat where
  typeInformation =
    Rel8.parseTypeInformation
      (maybeToRight "Could not parse Repeat" . parse)
      show
      Rel8.typeInformation

instance Rel8.DBEq Repeat
