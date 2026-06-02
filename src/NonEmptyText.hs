module NonEmptyText where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Rel8 qualified
import Relude hiding (show)
import Prelude (Show (..))

data NonEmptyText = NonEmptyText
  { firstChar :: Char,
    textValue :: Text
  }

parse :: Text -> Either String NonEmptyText
parse t = case Text.uncons t of
  Just (c, rest) -> Right $ NonEmptyText c rest
  Nothing -> Left "Expected text with at least one character!"

instance Semigroup NonEmptyText where
  NonEmptyText c1 t1 <> NonEmptyText c2 t2 = NonEmptyText c1 $ mconcat [t1, Text.singleton c2, t2]

instance ToText NonEmptyText where
  toText (NonEmptyText c rest) = Text.cons c rest

instance ToString NonEmptyText where
  toString = toString . toText

instance Rel8.DBType NonEmptyText where
  typeInformation =
    Rel8.parseTypeInformation
      parse
      toText
      Rel8.typeInformation

instance Rel8.DBEq NonEmptyText

instance Aeson.ToJSON NonEmptyText where
  toJSON = Aeson.String . toText

instance Aeson.FromJSON NonEmptyText where
  parseJSON = Aeson.withText "" $ either fail pure . parse

instance Show NonEmptyText where
  show = show . toText
