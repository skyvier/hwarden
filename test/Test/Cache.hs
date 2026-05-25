module Test.Cache (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent

tests :: TestTree
tests = testGroup "cache"
  [ testGroup "cacheAgeSeconds"
    [ testProperty "given a cache entry refreshed N seconds ago, cacheAgeSeconds returns N exactly" $
        propertyCacheAgeSecondsIsExact
    ]
  , testGroup
    "updateItemCacheState"
    [ testProperty "given any previous cache state, a successful refresh replaces it with a ready cache and success status" $
        propertyUpdateItemCacheStateSuccessReplacesState
    , testProperty "given a stale ready cache, a successful refresh replaces both cached items and stale failure metadata" $
        propertyUpdateItemCacheStateSuccessReplacesStaleMetadata
    , testProperty "given a ready cache, a failed refresh preserves cached items and records the latest refresh failure" $
        propertyUpdateItemCacheStateFailurePreservesReadyCache
    , testProperty "given no ready cache, a failed refresh leaves the cache unavailable with the new failure reason" $
        propertyUpdateItemCacheStateFailureWithoutReadyCache
    ]
  ]

propertyCacheAgeSecondsIsExact :: [Agent.ItemSummary] -> Agent.CacheAgeSeconds -> Property
propertyCacheAgeSecondsIsExact items cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
   in property $
        Agent.cacheAgeSeconds mockNow cacheEntry
          == cacheAgeSecondsValue

propertyUpdateItemCacheStateSuccessReplacesState ::
  Agent.ItemCacheState ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesState previousState cacheEntry =
  property $
    Agent.updateItemCacheState previousState (Right cacheEntry)
      == Agent.CacheReady cacheEntry Agent.LatestRefreshSucceeded

propertyUpdateItemCacheStateSuccessReplacesStaleMetadata ::
  Agent.CacheEntry ->
  Agent.CacheFillFailure ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesStaleMetadata oldCacheEntry oldFailure newCacheEntry =
  property $
    Agent.updateItemCacheState
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
    Agent.updateItemCacheState
      (Agent.CacheReady cacheEntry latestRefreshStatus)
      (Left cacheFillFailure)
      == Agent.CacheReady cacheEntry (Agent.LatestRefreshFailed cacheFillFailure)

propertyUpdateItemCacheStateFailureWithoutReadyCache ::
  Agent.CacheFillFailure ->
  Agent.CacheFillFailure ->
  Property
propertyUpdateItemCacheStateFailureWithoutReadyCache previousFailure newFailure =
  let notYetFilledResult =
        Agent.updateItemCacheState Agent.CacheNotYetFilled (Left newFailure)
      failedCacheFillResult =
        Agent.updateItemCacheState (Agent.CacheFillError previousFailure) (Left newFailure)
   in property $
        notYetFilledResult == Agent.CacheFillError newFailure
          && failedCacheFillResult == Agent.CacheFillError newFailure
