module Task where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import NonEmptyText (NonEmptyText)
import Relude hiding (repeat)

newtype Seconds = Seconds Integer
  deriving newtype (Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Task
  = TaskWithoutSubTasks (Task' Proxy)
  | TaskWithSubTasks (Task' Identity)
  deriving stock (Show, Generic)

type TaskWithoutSubTasks = Task' Proxy

type TaskWithSubTasks = Task' Identity

data Task' f = Task
  { description :: NonEmptyText,
    due :: Maybe UTCTime,
    remindAt :: Maybe UTCTime,
    repeatAfter :: Maybe Seconds,
    subTasks :: f (NonEmpty Task),
    tags :: Maybe (NonEmpty NonEmptyText)
  }
  deriving stock (Generic)

deriving instance Show TaskWithSubTasks

deriving instance Show TaskWithoutSubTasks

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True,
      Aeson.fieldLabelModifier = Aeson.camelTo2 '_',
      Aeson.sumEncoding = Aeson.UntaggedValue
    }

instance Aeson.ToJSON (Task' Proxy) where
  toJSON = Aeson.genericToJSON options

instance Aeson.FromJSON (Task' Proxy) where
  parseJSON = Aeson.genericParseJSON options

instance Aeson.ToJSON (Task' Identity) where
  toJSON = Aeson.genericToJSON options

instance Aeson.FromJSON (Task' Identity) where
  parseJSON = Aeson.genericParseJSON options

instance Aeson.ToJSON Task where
  toJSON = Aeson.genericToJSON options

instance Aeson.FromJSON Task where
  parseJSON = Aeson.genericParseJSON options

display :: Task -> Text
display = \case
  TaskWithSubTasks Task {..} ->
    unlines
      $ catMaybes
        [ Just "======== TASK BEGINS ========",
          Just $ "Description: " <> toText description
        ]
  TaskWithoutSubTasks Task {..} ->
    unlines
      $ catMaybes
        [ Just "======== TASK BEGINS ========",
          Just $ "Description: " <> toText description
        ]
