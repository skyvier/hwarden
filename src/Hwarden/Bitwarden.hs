{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Bitwarden
  ( Bitwarden (..),
    GetPasswordError (..),
    ListItemsError (..),
    SyncError (..),
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
import GHC.Generics (Generic)
import Hwarden.Types
  ( ItemSummary (ItemSummary),
    LoginItemId,
    Password,
    PasswordValue,
    SessionKey,
    Username
  )
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Instances.Text ()

data UnlockError
  = UnlockUnavailable
  | CodeRequired
  | UnlockFailed Text
  deriving (Eq, Show, Generic)

data ListItemsError
  = ListItemsUnavailable
  | ListItemsFailed Text
  deriving (Eq, Show, Generic)

data GetPasswordError
  = GetPasswordUnavailable
  | GetPasswordFailed Text
  deriving (Eq, Show, Generic)

data SyncError
  = SyncUnavailable
  | SyncFailed Text
  deriving (Eq, Show, Generic)

instance Arbitrary UnlockError where
  arbitrary =
    oneof
      [ pure UnlockUnavailable,
        pure CodeRequired,
        UnlockFailed . T.pack <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary ListItemsError where
  arbitrary =
    oneof
      [ pure ListItemsUnavailable,
        ListItemsFailed . T.pack <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary GetPasswordError where
  arbitrary =
    oneof
      [ pure GetPasswordUnavailable,
        GetPasswordFailed . T.pack <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary SyncError where
  arbitrary =
    oneof
      [ pure SyncUnavailable,
        SyncFailed . T.pack <$> arbitrary
      ]
  shrink = genericShrink

class Monad m => Bitwarden m where
  unlock :: Username -> Password -> m (Either UnlockError SessionKey)
  listItems :: SessionKey -> m (Either ListItemsError [ItemSummary])
  sync :: SessionKey -> m (Either SyncError ())
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
  { bwLoginUsername :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON BwLogin where
  parseJSON = withObject "BwLogin" $ \obj ->
    BwLogin <$> obj .:? "username"

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
            (maybe "" id (bwLoginUsername login))
        )
