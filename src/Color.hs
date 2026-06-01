module Color where

import Data.Colour.SRGB (Colour, sRGB24read)
import Relude
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types (ConsoleLayer (..), SGR (..))

redColour :: Colour Float
redColour = sRGB24read "#ff0000"

colour :: Colour Float -> String -> String
colour c s = mconcat [setSGRCode [SetRGBColor Foreground c], s, setSGRCode []]

red :: String -> String
red = colour redColour
