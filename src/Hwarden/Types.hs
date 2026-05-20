{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Types
  ( LoginItemId (..),
    ItemSummary (..),
    Password (..),
    PasswordValue (..),
    SessionKey (..),
    Username (..)
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (arbitrary), elements, listOf1)

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
  -- Keep generated session keys visually distinctive so secrecy tests can
  -- detect actual leaks instead of colliding with ordinary JSON digits or
  -- punctuation such as cache_age_seconds values.
  arbitrary =
    SessionKey . wrapNeedle . T.pack
      <$> listOf1 (elements sessionKeyAlphabet)

newtype Username = Username Text
  deriving (Eq, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"

newtype PasswordValue = PasswordValue Text
  deriving (Eq)

instance Show PasswordValue where
  show _ = "[REDACTED]"

instance Arbitrary PasswordValue where
  arbitrary = PasswordValue . T.pack <$> arbitrary

wrapNeedle :: Text -> Text
wrapNeedle value = "session-needle-" <> value <> "-end"

sessionKeyAlphabet :: [Char]
sessionKeyAlphabet =
  ['a' .. 'z']
    <> ['A' .. 'Z']
    <> ['0' .. '9']
