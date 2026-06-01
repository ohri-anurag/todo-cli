{-# LANGUAGE TemplateHaskell #-}

module TaskTest where

import Data.Time (utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NonEmptyText qualified
import Relude
import Repeat qualified
import Task qualified
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_display :: TestTree
test_display =
  goldenVsString "display" "test/golden/display.golden.txt"
    . pure
    . encodeUtf8
    . Task.display utc
    $ Task.TaskWithSubTasks
      Task.Task
        { description = $$(NonEmptyText.make "PARENT"),
          due = Just $ posixSecondsToUTCTime 1779453522,
          id = 1,
          remindAt = Just $ posixSecondsToUTCTime 1779451111,
          repeatAfter = Just Repeat.Daily,
          subTasks =
            Identity
              $ Task.TaskWithSubTasks
                Task.Task
                  { description = $$(NonEmptyText.make "CHILD"),
                    due = Just $ posixSecondsToUTCTime 1779453522,
                    id = 2,
                    remindAt = Just $ posixSecondsToUTCTime 1779451111,
                    repeatAfter = Just Repeat.Daily,
                    subTasks =
                      Identity
                        $ Task.TaskWithoutSubTasks
                          Task.Task
                            { description = $$(NonEmptyText.make "GRANDCHILD"),
                              due = Just $ posixSecondsToUTCTime 1779453522,
                              id = 3,
                              remindAt = Just $ posixSecondsToUTCTime 1779451111,
                              repeatAfter = Just Repeat.Daily,
                              subTasks = Proxy,
                              tags = Just $ $$(NonEmptyText.make "grandchild") :| []
                            }
                        :| [],
                    tags = Just $ $$(NonEmptyText.make "child") :| []
                  }
              :| [],
          tags = Just $ $$(NonEmptyText.make "parent") :| []
        }
