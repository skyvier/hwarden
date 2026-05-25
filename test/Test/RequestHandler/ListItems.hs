{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.ListItems (tests) where

import Data.Function ((&))
  
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent

tests :: TestTree 
tests = testGroup "list-items"
  [ testCase "given a locked state, a list-items request returns locked failure" $
      let currentState = Agent.Locked
          (newState, response, effects) =
            runMockBitwarden
              defaultMockEnv
              (Agent.handleRequestWith Agent.ListItems currentState)
       in do
            newState @?= currentState
            response @?= Agent.failureResponse "locked"
            effects @?= []
  , testProperty "given an unlocked state with a not-yet-filled cache, a list-items request returns cache unavailable and preserves state" $
      propertyHandleRequestWithListItemsNotYetFilled
  , testProperty "given an unlocked state with a failed cache fill, a list-items request returns cache unavailable and preserves state" $
      propertyHandleRequestWithListItemsFailedCacheFill
  , testProperty "given an unlocked state with cached items, a list-items request returns items and preserves state" $
      propertyHandleRequestWithListItemsPreservesState
  , testProperty "given cached items refreshed N seconds ago, a list-items request reports age N exactly" $
      propertyHandleRequestWithListItemsReportsExactCacheAge

  -- XXX: handleListItems instead of handleRequestWith
  , testProperty "given any initial state, handleListItems never changes the agent state" $
      propertyHandleListItemsPreservesState
  , testProperty "given a cache entry refreshed N seconds ago, handleListItems reports age N exactly" $
      propertyHandleListItemsReportsExactCacheAge
  ]

propertyHandleRequestWithListItemsNotYetFilled :: Agent.SessionKey -> Property
propertyHandleRequestWithListItemsNotYetFilled sessionKey =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.failureResponse "item cache unavailable"
          && effects == []

propertyHandleRequestWithListItemsFailedCacheFill ::
  Agent.SessionKey ->
  Agent.CacheFillFailure ->
  Property
propertyHandleRequestWithListItemsFailedCacheFill sessionKey cacheFillFailure =
  let currentState = Agent.Unlocked sessionKey (Agent.CacheFillError cacheFillFailure)
      (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.failureResponse "item cache unavailable"
          && effects == []

propertyHandleRequestWithListItemsPreservesState ::
  Agent.SessionKey ->
  Agent.CacheEntry ->
  Agent.LatestRefreshStatus ->
  Property
propertyHandleRequestWithListItemsPreservesState sessionKey cacheEntry latestRefreshStatus =
  let currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady cacheEntry latestRefreshStatus)
      items = Agent.cacheEntryItems cacheEntry
      (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withListItemsResult (Left (Agent.ListItemsFailed "list-items should not hit the backend")))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.itemListResponse items (Agent.cacheAgeSeconds mockNow cacheEntry)
          && effects == []

propertyHandleRequestWithListItemsReportsExactCacheAge ::
  Agent.SessionKey ->
  [Agent.ItemSummary] ->
  Agent.LatestRefreshStatus ->
  Agent.CacheAgeSeconds ->
  Property
propertyHandleRequestWithListItemsReportsExactCacheAge sessionKey items latestRefreshStatus cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
      currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady cacheEntry latestRefreshStatus)
      (_, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withListItemsResult (Left (Agent.ListItemsFailed "list-items should not hit the backend")))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        response == Agent.itemListResponse items cacheAgeSecondsValue
          && null effects

propertyHandleListItemsPreservesState ::
  Agent.CacheEntry ->
  Agent.AgentState ->
  Property
propertyHandleListItemsPreservesState cacheEntry initialState =
  let (newState, _, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleListItems cacheEntry initialState)
   in property (newState == initialState && effects == [])

propertyHandleListItemsReportsExactCacheAge ::
  [Agent.ItemSummary] ->
  Agent.AgentState ->
  Agent.CacheAgeSeconds ->
  Property
propertyHandleListItemsReportsExactCacheAge items initialState cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
      (_, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleListItems cacheEntry initialState)
   in property $
        response == Agent.itemListResponse items cacheAgeSecondsValue
          && null effects
