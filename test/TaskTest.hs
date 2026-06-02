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

test_display :: TestTree
test_display =
  goldenVsString "display" "test/golden/display.golden.txt"
    . pure
    . encodeUtf8
    . Task.display utc Setup.defaultPalette
    $ Task.TaskWithSubTasks
      Task.Task
        { description = (NonEmptyText 'P' "ARENT"),
          due = Just $ posixSecondsToUTCTime 1779453522,
          id = 1,
          remindAt = Just $ posixSecondsToUTCTime 1779451111,
          repeatAfter = Just Repeat.Daily,
          subTasks =
            Identity
              $ Task.TaskWithSubTasks
                Task.Task
                  { description = (NonEmptyText 'C' "HILD"),
                    due = Just $ posixSecondsToUTCTime 1779453522,
                    id = 2,
                    remindAt = Just $ posixSecondsToUTCTime 1779451111,
                    repeatAfter = Just Repeat.Daily,
                    subTasks =
                      Identity
                        $ Task.TaskWithoutSubTasks
                          Task.Task
                            { description = (NonEmptyText 'G' "RANDCHILD"),
                              due = Just $ posixSecondsToUTCTime 1779453522,
                              id = 3,
                              remindAt = Just $ posixSecondsToUTCTime 1779451111,
                              repeatAfter = Just Repeat.Daily,
                              subTasks = Proxy,
                              tags = Just $ (NonEmptyText 'g' "randchild") :| []
                            }
                        :| [],
                    tags = Just $ (NonEmptyText 'c' "hild") :| []
                  }
              :| [],
          tags = Just $ (NonEmptyText 'p' "arent") :| []
        }
