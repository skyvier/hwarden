{-# LANGUAGE OverloadedStrings #-}

module Test.Cache (tests) where

import Data.Function ((&))

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Cache
  ( cacheAgeSeconds,
    cacheFillFailureFromListItemsError,
    initialItemCacheState,
    syncErrorToCacheFillFailure,
    updateItemCacheState
  )

tests :: TestTree
tests =
  testGroup "cache"
    [ testGroup "cacheAgeSeconds"
        [ testProperty
            "given a cache entry refreshed N seconds ago, cacheAgeSeconds returns N exactly" $
            propertyCacheAgeSecondsIsExact
        ]
    , testGroup
        "initialItemCacheState"
        [ testProperty
            "given a successful unlock and successful sync plus list-items, the initial cache state is ready with a successful refresh status" $
            propertyInitialCacheStateDuringUnlockSuccess
        , testProperty
            "given a successful unlock and a failed sync, the initial cache state is a cache fill error" $
            propertyInitialCacheStateDuringUnlockSyncFailure
        , testProperty
            "given a successful unlock and a successful sync but failed list-items, the initial cache state is a cache fill error for the list-items failure" $
            propertyInitialCacheStateDuringUnlockListItemsFailure
        ]
    , testGroup
        "updateItemCacheState"
        [ testProperty
            "given any previous cache state, a successful refresh replaces it with a ready cache and success status" $
            propertyUpdateItemCacheStateSuccessReplacesState
        , testProperty
            "given a stale ready cache, a successful refresh replaces both cached items and stale failure metadata" $
            propertyUpdateItemCacheStateSuccessReplacesStaleMetadata
        , testProperty
            "given a ready cache, a failed refresh preserves cached items and records the latest refresh failure" $
            propertyUpdateItemCacheStateFailurePreservesReadyCache
        , testProperty
            "given no ready cache, a failed refresh leaves the cache unavailable with the new failure reason" $
            propertyUpdateItemCacheStateFailureWithoutReadyCache
        ]
    ]

propertyCacheAgeSecondsIsExact :: [Agent.ItemSummary] -> Agent.CacheAgeSeconds -> Property
propertyCacheAgeSecondsIsExact items cacheAgeSecondsValue =
  let
    cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
  in
    property $
      cacheAgeSeconds mockNow cacheEntry
        == cacheAgeSecondsValue

propertyInitialCacheStateDuringUnlockSuccess
  :: Agent.SessionKey
  -> [Agent.ItemSummary]
  -> MockEnv
  -> Property
propertyInitialCacheStateDuringUnlockSuccess sessionKey items mockEnv =
  let
    request =
      Agent.UnlockRequest
        (Agent.Username "me@example.com")
        (Agent.Password "secret")
    expectedCacheEntry = Agent.CacheEntry items mockNow
    (newState, _, _) =
      runMockBitwarden
        ( mockEnv
            & withUnlockResult (Right sessionKey)
            & withSyncResult (Right ())
            & withListItemsResult (Right items)
        )
        (Agent.handleRequestWith request Agent.Locked)
  in
    property $
      case newState of
        Agent.Unlocked unlockedSessionKey itemCacheState ->
          unlockedSessionKey == sessionKey
            && itemCacheState == initialItemCacheState (Right expectedCacheEntry)
        Agent.Locked ->
          False

propertyInitialCacheStateDuringUnlockSyncFailure
  :: Agent.SessionKey
  -> Bitwarden.SyncError
  -> Either Agent.ListItemsError [Agent.ItemSummary]
  -> MockEnv
  -> Property
propertyInitialCacheStateDuringUnlockSyncFailure
  sessionKey
  syncError
  initialListItemsResult
  mockEnv =
  let
    request =
      Agent.UnlockRequest
        (Agent.Username "me@example.com")
        (Agent.Password "secret")
    expectedFailure =
      syncErrorToCacheFillFailure sessionKey syncError
    (newState, _, _) =
      runMockBitwarden
        ( mockEnv
            & withUnlockResult (Right sessionKey)
            & withSyncResult (Left syncError)
            & withListItemsResult initialListItemsResult
        )
        (Agent.handleRequestWith request Agent.Locked)
  in
    property $
      case newState of
        Agent.Unlocked unlockedSessionKey itemCacheState ->
          unlockedSessionKey == sessionKey
            && itemCacheState == initialItemCacheState (Left expectedFailure)
        Agent.Locked ->
          False

propertyInitialCacheStateDuringUnlockListItemsFailure
  :: Agent.SessionKey
  -> Agent.ListItemsError
  -> MockEnv
  -> Property
propertyInitialCacheStateDuringUnlockListItemsFailure
  sessionKey
  listItemsError
  mockEnv =
  let
    request =
      Agent.UnlockRequest
        (Agent.Username "me@example.com")
        (Agent.Password "secret")
    expectedFailure =
      cacheFillFailureFromListItemsError sessionKey listItemsError
    (newState, _, _) =
      runMockBitwarden
        ( mockEnv
            & withUnlockResult (Right sessionKey)
            & withSyncResult (Right ())
            & withListItemsResult (Left listItemsError)
        )
        (Agent.handleRequestWith request Agent.Locked)
  in
    property $
      case newState of
        Agent.Unlocked unlockedSessionKey itemCacheState ->
          unlockedSessionKey == sessionKey
            && itemCacheState == initialItemCacheState (Left expectedFailure)
        Agent.Locked ->
          False

propertyUpdateItemCacheStateSuccessReplacesState ::
  Agent.ItemCacheState ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesState previousState cacheEntry =
  property $
    updateItemCacheState previousState (Right cacheEntry)
      == Agent.CacheReady cacheEntry Agent.LatestRefreshSucceeded

propertyUpdateItemCacheStateSuccessReplacesStaleMetadata ::
  Agent.CacheEntry ->
  Agent.CacheFillFailure ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesStaleMetadata oldCacheEntry oldFailure newCacheEntry =
  property $
    updateItemCacheState
      (Agent.CacheReady oldCacheEntry (Agent.LatestRefreshFailed oldFailure))
      (Right newCacheEntry)
      == Agent.CacheReady newCacheEntry Agent.LatestRefreshSucceeded

propertyUpdateItemCacheStateFailurePreservesReadyCache ::
  Agent.CacheEntry ->
  Agent.LatestRefreshStatus ->
  Agent.CacheFillFailure ->
  Property
propertyUpdateItemCacheStateFailurePreservesReadyCache cacheEntry latestRefreshStatus cacheFillFailure =
  property $
    updateItemCacheState
      (Agent.CacheReady cacheEntry latestRefreshStatus)
      (Left cacheFillFailure)
      == Agent.CacheReady cacheEntry (Agent.LatestRefreshFailed cacheFillFailure)

propertyUpdateItemCacheStateFailureWithoutReadyCache ::
  Agent.CacheFillFailure ->
  Agent.CacheFillFailure ->
  Property
propertyUpdateItemCacheStateFailureWithoutReadyCache previousFailure newFailure =
  let
    notYetFilledResult =
      updateItemCacheState Agent.CacheNotYetFilled (Left newFailure)
    failedCacheFillResult =
      updateItemCacheState
        (Agent.CacheFillError previousFailure)
        (Left newFailure)
  in
    property $
      notYetFilledResult == Agent.CacheFillError newFailure
        && failedCacheFillResult == Agent.CacheFillError newFailure
