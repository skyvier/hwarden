{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Types
  ( LoginItemId (..),
    ItemSummary (..),
    Password (..),
    PasswordValue (..),
    TwoFactorCode (..),
    SessionKey (..),
    Username (..)
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (arbitrary))

newtype LoginItemId = LoginItemId Text
  deriving (Eq, Show)

instance Arbitrary LoginItemId where
  arbitrary = LoginItemId . T.pack <$> arbitrary

data ItemSummary = ItemSummary
  { itemId :: Text,
    itemName :: Text,
    itemUsername :: Text
  }
  deriving (Eq, Show)

instance ToJSON ItemSummary where
  toJSON (ItemSummary summaryId summaryName summaryUsername) =
    object
      [ "id" .= summaryId,
        "name" .= summaryName,
        "username" .= summaryUsername
      ]

instance FromJSON ItemSummary where
  parseJSON = withObject "ItemSummary" $ \obj ->
    ItemSummary <$> obj .: "id" <*> obj .: "name" <*> obj .: "username"

instance Arbitrary ItemSummary where
  arbitrary = ItemSummary <$> (T.pack <$> arbitrary) <*> (T.pack <$> arbitrary) <*> (T.pack <$> arbitrary)

newtype SessionKey = SessionKey Text
  deriving (Eq)

instance Show SessionKey where
  show _ = "[REDACTED]"

instance Arbitrary SessionKey where
  arbitrary = SessionKey . T.pack <$> arbitrary

newtype Username = Username Text
  deriving (Eq, Show)

instance Arbitrary Username where
  arbitrary = Username . T.pack <$> arbitrary

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"

instance Arbitrary Password where
  arbitrary = Password . T.pack <$> arbitrary

newtype PasswordValue = PasswordValue Text
  deriving (Eq)

instance Show PasswordValue where
  show _ = "[REDACTED]"

newtype TwoFactorCode = TwoFactorCode Text
  deriving (Eq)

instance Show TwoFactorCode where
  show _ = "[REDACTED]"

instance Arbitrary PasswordValue where
  arbitrary = PasswordValue . T.pack <$> arbitrary

instance Arbitrary TwoFactorCode where
  arbitrary = TwoFactorCode . T.pack <$> arbitrary
