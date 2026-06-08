module CommandTest where

import Command (Command (..))
import Data.Aeson qualified as Aeson
import Relude
import Test.Tasty.Golden.Extra.GoldenVsToJSON (GoldenVsToJSON (..))

tasty_command :: GoldenVsToJSON
tasty_command =
  GoldenVsToJSON ("test/golden/Command.golden.json")
    $ Aeson.eitherDecodeFileStrict @Command ("test/golden/Command.json")
