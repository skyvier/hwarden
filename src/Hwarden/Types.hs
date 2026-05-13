{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Types
  ( ItemSummary (..),
    Password (..),
    SessionKey (..),
    Username (..)
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (arbitrary))

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

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"
