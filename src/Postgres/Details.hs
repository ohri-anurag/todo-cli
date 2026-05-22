module Postgres.Details where

import Data.Aeson qualified as Aeson
import NonEmptyText (NonEmptyText)
import Relude

data Details = Details
  { table :: NonEmptyText,
    schema :: NonEmptyText,
    connString :: NonEmptyText
  }
  deriving stock (Show, Generic)

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON options

instance Aeson.FromJSON Details where
  parseJSON = Aeson.genericParseJSON options
