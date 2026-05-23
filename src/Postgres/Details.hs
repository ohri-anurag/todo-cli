module Postgres.Details where

import Data.Aeson qualified as Aeson
import NonEmptyText (NonEmptyText)
import Relude

newtype TableName = TableName NonEmptyText
  deriving newtype (Show, Aeson.ToJSON, Aeson.FromJSON)

newtype Schema = Schema NonEmptyText
  deriving newtype (Show, Aeson.ToJSON, Aeson.FromJSON)

data Details = Details
  { table :: TableName,
    schema :: Schema,
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
