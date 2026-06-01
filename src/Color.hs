module Color where

import Data.Colour.SRGB (Colour, sRGB24read)
import Relude
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types (ConsoleIntensity (..), ConsoleLayer (..), SGR (..))

colour :: Colour Float -> Text -> Text
colour c s =
  mconcat
    [ toText $ setSGRCode [SetConsoleIntensity BoldIntensity, SetRGBColor Foreground c],
      s,
      toText $ setSGRCode []
    ]

red :: Text -> Text
red = colour (sRGB24read "#aa0000")

green :: Text -> Text
green = colour (sRGB24read "#00aa00")
