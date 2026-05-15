{-# LANGUAGE TemplateHaskell #-}

module NonEmptyText where

import Data.Aeson qualified as Aeson
import Language.Haskell.TH (Code, Quote)
import Refined (refine, unrefine)
import Refined qualified
import Refined.Unsafe qualified as Refined
import Rel8 qualified
import Relude

newtype NonEmptyText = NonEmptyText
  { unNonEmptyText :: Refined.Refined Refined.NonEmpty Text
  }
  deriving newtype (Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

make :: (MonadFail m, Quote m) => Text -> Code m NonEmptyText
make t = [||NonEmptyText $$(Refined.refineTH t)||]

instance Semigroup NonEmptyText where
  NonEmptyText a <> NonEmptyText b =
    NonEmptyText
      $ Refined.unsafeRefine
      $ Refined.unrefine a
      <> Refined.unrefine b

instance Rel8.DBType NonEmptyText where
  typeInformation =
    Rel8.parseTypeInformation
      (bimap show NonEmptyText . refine)
      (unrefine . unNonEmptyText)
      Rel8.typeInformation

instance Rel8.DBEq NonEmptyText
