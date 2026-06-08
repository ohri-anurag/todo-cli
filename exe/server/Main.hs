module Main where

import AWS.Lambda.Runtime (ioRuntime)
import Data.Aeson (Value)
import Relude

todoHandler :: Value -> IO (Either String Text)
todoHandler _ = pure $ Right ""

main :: IO ()
main = ioRuntime todoHandler
