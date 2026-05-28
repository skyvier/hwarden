{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics (Generic)
import Test.QuickCheck (elements, listOf1)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Instances.Text ()

newtype LoginItemId = LoginItemId Text
  deriving (Eq, Show, Generic)

instance Arbitrary LoginItemId where
  arbitrary = LoginItemId . T.pack <$> arbitrary
  shrink = genericShrink

data ItemSummary = ItemSummary
  { itemId :: Text,
    itemName :: Text,
    itemUsername :: Text
  }
  deriving (Eq, Show, Generic)

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
  shrink = genericShrink

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
  shrink (SessionKey value) =
    SessionKey . wrapNeedle
      <$> shrinkNeedlePayload sessionNeedlePrefix sessionNeedleSuffix value

newtype Username = Username Text
  deriving (Eq, Show, Generic)

instance Arbitrary Username where
  arbitrary = Username . T.pack <$> arbitrary
  shrink = genericShrink

newtype Password = Password Text
  deriving (Eq, Generic)

instance Show Password where
  show _ = "[REDACTED]"

instance Arbitrary Password where
  arbitrary =
    Password . wrapPasswordNeedle . T.pack
      <$> listOf1 (elements passwordAlphabet)
  shrink (Password value) =
    Password . wrapPasswordNeedle
      <$> shrinkNeedlePayload passwordNeedlePrefix passwordNeedleSuffix value

newtype PasswordValue = PasswordValue Text
  deriving (Eq, Generic)

instance Show PasswordValue where
  show _ = "[REDACTED]"

instance Arbitrary PasswordValue where
  arbitrary = PasswordValue . T.pack <$> arbitrary
  shrink = genericShrink

wrapNeedle :: Text -> Text
wrapNeedle value = sessionNeedlePrefix <> value <> sessionNeedleSuffix

wrapPasswordNeedle :: Text -> Text
wrapPasswordNeedle value =
  passwordNeedlePrefix <> value <> passwordNeedleSuffix

sessionNeedlePrefix :: Text
sessionNeedlePrefix = "session-needle-"

sessionNeedleSuffix :: Text
sessionNeedleSuffix = "-end"

passwordNeedlePrefix :: Text
passwordNeedlePrefix = "password-needle-"

passwordNeedleSuffix :: Text
passwordNeedleSuffix = "-end"

shrinkNeedlePayload :: Text -> Text -> Text -> [Text]
shrinkNeedlePayload prefix suffix value =
  case T.stripPrefix prefix value >>= T.stripSuffix suffix of
    Just payload ->
      T.pack <$> shrink (T.unpack payload)
    Nothing ->
      []

sessionKeyAlphabet :: [Char]
sessionKeyAlphabet =
  ['a' .. 'z']
    <> ['A' .. 'Z']
    <> ['0' .. '9']

passwordAlphabet :: [Char]
passwordAlphabet =
  ['a' .. 'z']
    <> ['A' .. 'Z']
    <> ['0' .. '9']
