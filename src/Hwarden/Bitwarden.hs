{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Bitwarden
  ( Bitwarden (..),
    GetPasswordError (..),
    ListItemsError (..),
    UnlockError (..),
    BwItem (..),
    BwLogin (..),
    defaultBitwardenServerUrl,
    determineBitwardenServerUrl,
    extractLoginItems
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Types
  ( ItemSummary (ItemSummary),
    LoginItemId,
    Password,
    PasswordValue,
    SessionKey,
    Username
  )
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data UnlockError
  = UnlockUnavailable
  | UnlockFailed Text
  deriving (Eq, Show)

data ListItemsError
  = ListItemsUnavailable
  | ListItemsFailed Text
  deriving (Eq, Show)

data GetPasswordError
  = GetPasswordUnavailable
  | GetPasswordFailed Text
  deriving (Eq, Show)

instance Arbitrary UnlockError where
  arbitrary =
    oneof
      [ pure UnlockUnavailable,
        UnlockFailed . T.pack <$> arbitrary
      ]

instance Arbitrary ListItemsError where
  arbitrary =
    oneof
      [ pure ListItemsUnavailable,
        ListItemsFailed . T.pack <$> arbitrary
      ]

instance Arbitrary GetPasswordError where
  arbitrary =
    oneof
      [ pure GetPasswordUnavailable,
        GetPasswordFailed . T.pack <$> arbitrary
      ]

class Monad m => Bitwarden m where
  unlock :: Username -> Password -> m (Either UnlockError SessionKey)
  listItems :: SessionKey -> m (Either ListItemsError [ItemSummary])
  getPassword :: SessionKey -> LoginItemId -> m (Either GetPasswordError PasswordValue)

defaultBitwardenServerUrl :: Text
defaultBitwardenServerUrl = "https://vault.bitwarden.eu"

determineBitwardenServerUrl :: Maybe String -> Text
determineBitwardenServerUrl = maybe defaultBitwardenServerUrl T.pack

data BwItem = BwItem
  { bwItemId :: Text,
    bwItemName :: Text,
    bwItemLogin :: Maybe BwLogin
  }
  deriving (Eq, Show)

instance FromJSON BwItem where
  parseJSON = withObject "BwItem" $ \obj ->
    BwItem <$> obj .: "id" <*> obj .: "name" <*> obj .:? "login"

data BwLogin = BwLogin
  { bwLoginUsername :: Text
  }
  deriving (Eq, Show)

instance FromJSON BwLogin where
  parseJSON = withObject "BwLogin" $ \obj ->
    BwLogin <$> obj .: "username"

extractLoginItems :: [BwItem] -> [ItemSummary]
extractLoginItems = mapMaybe toItemSummary

toItemSummary :: BwItem -> Maybe ItemSummary
toItemSummary item =
  case bwItemLogin item of
    Nothing -> Nothing
    Just login ->
      Just
        ( ItemSummary
            (bwItemId item)
            (bwItemName item)
            (bwLoginUsername login)
        )
