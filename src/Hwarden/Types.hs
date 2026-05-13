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
import Test.QuickCheck (Arbitrary (arbitrary), elements, listOf1)

data ItemSummary = ItemSummary
  { itemId :: Text,
    itemName :: Text,
    itemUsername :: Text
  }
  deriving (Eq, Show)

instance ToJSON ItemSummary where
  toJSON (ItemSummary itemId itemName itemUsername) =
    object
      [ "id" .= itemId,
        "name" .= itemName,
        "username" .= itemUsername
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
  arbitrary =
    SessionKey . ("session-key-" <>) . T.pack <$> listOf1 (elements sessionChars)

sessionChars :: [Char]
sessionChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

newtype Username = Username Text
  deriving (Eq, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"
