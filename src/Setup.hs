module Setup where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import NonEmptyText (NonEmptyText)
import Postgres.Details qualified as Postgres
import Relude hiding (id, one, repeat)

newtype TableName = TableName NonEmptyText
  deriving newtype (Show, Aeson.ToJSON, Aeson.FromJSON)

newtype Schema = Schema NonEmptyText
  deriving newtype (Show, Aeson.ToJSON, Aeson.FromJSON)

data Palette = Palette
  { id :: Colour Float,
    description :: Colour Float,
    due :: Colour Float,
    remindAt :: Colour Float,
    repeat :: Colour Float,
    tags :: Colour Float,
    subTasks :: Colour Float
  }

defaultPalette :: Palette
defaultPalette =
  Palette
    { id = sRGB24read "#00aa00",
      description = sRGB24read "#00aaaa",
      due = sRGB24read "#aaaa00",
      remindAt = sRGB24read "#faa306",
      repeat = sRGB24read "#ed7ffc",
      tags = sRGB24read "#7ffcc0",
      subTasks = sRGB24read "#a37ffc"
    }

instance Aeson.ToJSON Palette where
  toJSON Palette {..} =
    Aeson.object
      [ "id" .= sRGB24show id,
        "description" .= sRGB24show description,
        "due" .= sRGB24show due,
        "remind_at" .= sRGB24show remindAt,
        "repeat" .= sRGB24show repeat,
        "tags" .= sRGB24show tags,
        "sub_tasks" .= sRGB24show subTasks
      ]

instance Aeson.FromJSON Palette where
  parseJSON = Aeson.withObject "Palette" $ \o -> do
    id <- sRGB24read <$> o .: "id"
    description <- sRGB24read <$> o .: "description"
    due <- sRGB24read <$> o .: "due"
    remindAt <- sRGB24read <$> o .: "remind_at"
    repeat <- sRGB24read <$> o .: "repeat"
    tags <- sRGB24read <$> o .: "tags"
    subTasks <- sRGB24read <$> o .: "sub_tasks"
    pure Palette {..}

data Details = Details
  { postgres :: Postgres.Details,
    palette :: Palette
  }
  deriving stock (Generic)

defaultDetails :: Details
defaultDetails =
  Details
    { postgres = Postgres.defaultDetails,
      palette = defaultPalette
    }

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON options

instance Aeson.FromJSON Details where
  parseJSON = Aeson.genericParseJSON options
