module TaskTest where

import Data.Time (utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText (NonEmptyText (..))
import Relude
import Repeat qualified
import Setup qualified
import Task qualified
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))

test_display :: TestTree
test_display =
  goldenVsString "display" "test/golden/display.golden.txt"
    . pure
    . encodeUtf8
    . Task.display utc Setup.defaultPalette
    $ Task.Task
      { description = (NonEmptyText 'P' "ARENT"),
        due = Just $ posixSecondsToUTCTime 1779453522,
        id = 1,
        remindAt = Just $ posixSecondsToUTCTime 1779451111,
        repeatAfter = Just Repeat.Daily,
        subTasks =
          Just
            $ Task.Task
              { description = (NonEmptyText 'C' "HILD"),
                due = Just $ posixSecondsToUTCTime 1779453522,
                id = 2,
                remindAt = Just $ posixSecondsToUTCTime 1779451111,
                repeatAfter = Just Repeat.Daily,
                subTasks =
                  Just
                    $ Task.Task
                      { description = (NonEmptyText 'G' "RANDCHILD"),
                        due = Just $ posixSecondsToUTCTime 1779453522,
                        id = 3,
                        remindAt = Just $ posixSecondsToUTCTime 1779451111,
                        repeatAfter = Just Repeat.Daily,
                        subTasks = Nothing,
                        tags = Just $ (NonEmptyText 'g' "randchild") :| []
                      }
                    :| [],
                tags = Just $ (NonEmptyText 'c' "hild") :| []
              }
            :| [],
        tags = Just $ (NonEmptyText 'p' "arent") :| []
      }

unit_getTaskAndAllSubTasks :: Assertion
unit_getTaskAndAllSubTasks = do
  let grandparent =
        Task.Task
          { description = (NonEmptyText 'P' "ARENT"),
            due = Just $ posixSecondsToUTCTime 1779453522,
            id = 1,
            remindAt = Just $ posixSecondsToUTCTime 1779451111,
            repeatAfter = Just Repeat.Daily,
            subTasks = Just $ parent :| [],
            tags = Just $ (NonEmptyText 'p' "arent") :| []
          }
      parent =
        Task.Task
          { description = (NonEmptyText 'C' "HILD"),
            due = Just $ posixSecondsToUTCTime 1779453522,
            id = 2,
            remindAt = Just $ posixSecondsToUTCTime 1779451111,
            repeatAfter = Just Repeat.Daily,
            subTasks = Just $ child :| [],
            tags = Just $ (NonEmptyText 'c' "hild") :| []
          }
      child =
        Task.Task
          { description = (NonEmptyText 'G' "RANDCHILD"),
            due = Just $ posixSecondsToUTCTime 1779453522,
            id = 3,
            remindAt = Just $ posixSecondsToUTCTime 1779451111,
            repeatAfter = Just Repeat.Daily,
            subTasks = Nothing,
            tags = Just $ (NonEmptyText 'g' "randchild") :| []
          }
  Task.getTaskAndAllSubTasks grandparent @?= [grandparent, parent, child]
