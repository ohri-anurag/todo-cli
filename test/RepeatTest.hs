module RepeatTest where

import Refined.Unsafe (unsafeRefine)
import Relude
import Repeat qualified
import Test.Tasty.HUnit (Assertion, (@?=))

tripping ::
  (Applicative f, Eq (f a), Show (f a)) =>
  (a -> b) ->
  (b -> f a) ->
  a ->
  Assertion
tripping to from a = from (to a) @?= pure a

unit_trippingRepeat :: Assertion
unit_trippingRepeat = do
  traverse_
    (tripping show Repeat.parse)
    [ Repeat.Daily,
      Repeat.Weekly,
      Repeat.Monthly,
      Repeat.Yearly,
      Repeat.Custom (unsafeRefine 2) Repeat.Days,
      Repeat.CustomWeekly
        Repeat.CustomWeek
          { mon = False,
            tue = True,
            wed = False,
            thu = True,
            fri = False,
            sat = True,
            sun = False
          }
    ]
