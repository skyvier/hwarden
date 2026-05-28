{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Cache
  ( CacheAgeSeconds (..),
    CacheFillFailure (..),
    CacheEntry (..),
    ItemCacheState (..),
    LatestRefreshStatus (..),
    cacheAgeSeconds,
    initialItemCacheState,
    updateItemCacheState,
    cacheFillFailureFromListItemsError,
    syncErrorToCacheFillFailure,
    buildInitialCacheState,
    refreshCacheEntry,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Time (MonadTime, currentTime)

import GHC.Generics (Generic)

import Test.QuickCheck (Gen, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Instances.Time ()

import Hwarden.Bitwarden
  ( Bitwarden (listItems, sync),
    ListItemsError (..),
    SyncError (..)
  )
import Hwarden.Response (CacheAgeSeconds (..))
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (SessionSecret),
    sanitizeListItemsFailure,
    sanitizeSyncFailure
  )
import Hwarden.Types (ItemSummary, SessionKey (..))

data LatestRefreshStatus
  = LatestRefreshSucceeded
  | LatestRefreshFailed CacheFillFailure
  deriving (Eq, Show, Generic)

data CacheFillFailure
  = CacheFillUnavailable
  | CacheFillFailed (SanitizedText SessionSecret)
  deriving (Eq, Show, Generic)

instance Arbitrary CacheFillFailure where
  arbitrary =
    oneof
      [ pure CacheFillUnavailable,
        CacheFillFailed <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary LatestRefreshStatus where
  arbitrary =
    oneof
      [ pure LatestRefreshSucceeded,
        LatestRefreshFailed <$> arbitrary
      ]
  shrink = genericShrink

data CacheEntry = CacheEntry
  { cacheEntryItems :: [ItemSummary],
    cacheEntryRefreshedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance Arbitrary CacheEntry where
  arbitrary =
    CacheEntry <$> arbitrary <*> arbitraryUtcTime
  shrink = genericShrink

data ItemCacheState
  = CacheNotYetFilled
  | CacheFillError CacheFillFailure
  | CacheReady CacheEntry LatestRefreshStatus
  deriving (Eq, Show, Generic)

instance Arbitrary ItemCacheState where
  arbitrary =
    oneof
      [ pure CacheNotYetFilled,
        CacheFillError <$> arbitrary,
        CacheReady <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

arbitraryUtcTime :: Gen UTCTime
arbitraryUtcTime =
  posixSecondsToUTCTime . fromInteger . abs <$> arbitrary

cacheAgeSeconds :: UTCTime -> CacheEntry -> CacheAgeSeconds
cacheAgeSeconds now cacheEntry =
  CacheAgeSeconds (floor (diffUTCTime now (cacheEntryRefreshedAt cacheEntry)))

initialItemCacheState :: Either CacheFillFailure CacheEntry -> ItemCacheState
initialItemCacheState refreshResult =
  case refreshResult of
    Right cacheEntry ->
      CacheReady cacheEntry LatestRefreshSucceeded
    Left cacheFillFailure ->
      CacheFillError cacheFillFailure

updateItemCacheState :: ItemCacheState -> Either CacheFillFailure CacheEntry -> ItemCacheState
updateItemCacheState itemCacheState refreshResult =
  case (itemCacheState, refreshResult) of
    (_, Right cacheEntry) ->
      CacheReady cacheEntry LatestRefreshSucceeded
    (CacheReady cacheEntry _, Left cacheFillFailure) ->
      CacheReady cacheEntry (LatestRefreshFailed cacheFillFailure)
    (_, Left cacheFillFailure) ->
      CacheFillError cacheFillFailure

cacheFillFailureFromListItemsError :: SessionKey -> ListItemsError -> CacheFillFailure
cacheFillFailureFromListItemsError sessionKey listItemsFailure =
  case listItemsFailure of
    ListItemsUnavailable ->
      CacheFillUnavailable
    ListItemsFailed err ->
      listItemsCacheFillFailure sessionKey err

syncErrorToCacheFillFailure :: SessionKey -> SyncError -> CacheFillFailure
syncErrorToCacheFillFailure sessionKey SyncUnavailable =
  syncCacheFillFailure sessionKey "bw sync was unavailable"
syncErrorToCacheFillFailure sessionKey (SyncFailed errMsg) =
  syncCacheFillFailure sessionKey ("bw sync failed due to " <> errMsg)

buildInitialCacheState :: (Bitwarden m, MonadTime m) => SessionKey -> m ItemCacheState
buildInitialCacheState sessionKey =
  initialItemCacheState <$> refreshCacheEntry sessionKey

refreshCacheEntry :: (Bitwarden m, MonadTime m) => SessionKey -> m (Either CacheFillFailure CacheEntry)
refreshCacheEntry sessionKey = runExceptT $ do
  ExceptT $ first (syncErrorToCacheFillFailure sessionKey) <$> sync sessionKey
  items <-
    ExceptT $ first (cacheFillFailureFromListItemsError sessionKey) <$> listItems sessionKey
  now <- currentTime
  pure $ CacheEntry items now

listItemsCacheFillFailure :: SessionKey -> Text -> CacheFillFailure
listItemsCacheFillFailure sessionKey err =
  CacheFillFailed (sanitizeListItemsFailure sessionKey err)

syncCacheFillFailure :: SessionKey -> Text -> CacheFillFailure
syncCacheFillFailure sessionKey err =
  CacheFillFailed (sanitizeSyncFailure sessionKey err)
