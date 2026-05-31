module Repeat where

import Data.Text qualified as Text
import Refined (GreaterThan, Refined, refineFail, unrefine)
import Rel8 qualified
import Relude hiding (show, words)
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
        [ if mon then Just "mon" else Nothing,
          if tue then Just "tue" else Nothing,
          if wed then Just "wed" else Nothing,
          if thu then Just "thu" else Nothing,
          if fri then Just "fri" else Nothing,
          if sat then Just "sat" else Nothing,
          if sun then Just "sun" else Nothing
        ]

data Repeat
  = Daily
  | Weekly
  | Monthly
  | Yearly
  | Custom (Refined (GreaterThan 1) Int) RepeatTime
  | CustomWeekly CustomWeek
  deriving (Eq)

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
        days = Text.splitOn "," text
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

instance Rel8.DBType Repeat where
  typeInformation =
    Rel8.parseTypeInformation
      (maybeToRight "Could not parse Repeat" . parse)
      show
      Rel8.typeInformation

instance Rel8.DBEq Repeat
