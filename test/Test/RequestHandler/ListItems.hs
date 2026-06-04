{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.ListItems (tests) where

import Control.Monad.Time (MonadTime (..))
import Data.Function ((&))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Cache (cacheAgeSeconds)

tests :: TestTree
tests =
  testGroup
    "list-items"
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
    , testProperty "given a ready cache with a failed latest refresh, a list-items request returns cached items" $
        propertyHandleRequestWithListItemsServesStaleCache
    , testCase "given an unlocked state with an empty ready cache, a list-items request returns an empty item list" $
        testHandleRequestWithListItemsEmptyCache
    , testProperty "given any initial state, a list-items request never calls the Bitwarden backend" $
        propertyHandleRequestWithListItemsDoesNotCallBackend
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
          && response == Agent.itemListResponse items (cacheAgeSeconds mockNow cacheEntry) (Agent.cacheRefreshStatusFromLatest latestRefreshStatus)
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
        response == Agent.itemListResponse items cacheAgeSecondsValue (Agent.cacheRefreshStatusFromLatest latestRefreshStatus)
          && null effects

propertyHandleRequestWithListItemsServesStaleCache ::
  Agent.SessionKey ->
  Agent.CacheEntry ->
  Agent.CacheFillFailure ->
  Property
propertyHandleRequestWithListItemsServesStaleCache sessionKey cacheEntry cacheFillFailure =
  let currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady cacheEntry (Agent.LatestRefreshFailed cacheFillFailure))
      items = Agent.cacheEntryItems cacheEntry
      (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withListItemsResult (Left (Agent.ListItemsFailed "list-items should not hit the backend")))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.itemListResponse items (cacheAgeSeconds mockNow cacheEntry) Agent.CacheRefreshFailed
          && effects == []

testHandleRequestWithListItemsEmptyCache :: Assertion
testHandleRequestWithListItemsEmptyCache =
  let cacheEntry = Agent.CacheEntry [] mockNow
      currentState =
        Agent.Unlocked
          (Agent.SessionKey "session-key")
          (Agent.CacheReady cacheEntry Agent.LatestRefreshSucceeded)
      (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.ListItems currentState)
   in do
        newState @?= currentState
        response @?= Agent.itemListResponse [] (Agent.CacheAgeSeconds 0) Agent.CacheRefreshSucceeded
        effects @?= []

propertyHandleRequestWithListItemsDoesNotCallBackend ::
  Agent.AgentState ->
  Property
propertyHandleRequestWithListItemsDoesNotCallBackend initialState =
  let (_, backendCalls) =
        runCallCountingBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.ListItems initialState)
   in property (backendCalls == noBackendCalls)

data BackendCalls = BackendCalls
  { unlockCalls :: Int
  , syncCalls :: Int
  , listItemsCalls :: Int
  , getPasswordCalls :: Int
  }
  deriving (Eq, Show)

noBackendCalls :: BackendCalls
noBackendCalls =
  BackendCalls
    { unlockCalls = 0
    , syncCalls = 0
    , listItemsCalls = 0
    , getPasswordCalls = 0
    }

newtype CallCountingBitwarden a = CallCountingBitwarden
  { runCallCountingBitwardenInternal :: MockEnv -> BackendCalls -> (a, BackendCalls)
  }

instance Functor CallCountingBitwarden where
  fmap f (CallCountingBitwarden run) =
    CallCountingBitwarden $ \mockEnv backendCalls ->
      let (value, updatedCalls) = run mockEnv backendCalls
       in (f value, updatedCalls)

instance Applicative CallCountingBitwarden where
  pure value = CallCountingBitwarden $ \_ backendCalls -> (value, backendCalls)
  CallCountingBitwarden apply <*> CallCountingBitwarden run =
    CallCountingBitwarden $ \mockEnv backendCalls ->
      let (f, appliedCalls) = apply mockEnv backendCalls
          (value, updatedCalls) = run mockEnv appliedCalls
       in (f value, updatedCalls)

instance Monad CallCountingBitwarden where
  CallCountingBitwarden run >>= next =
    CallCountingBitwarden $ \mockEnv backendCalls ->
      let (value, updatedCalls) = run mockEnv backendCalls
          CallCountingBitwarden runNext = next value
       in runNext mockEnv updatedCalls

instance Bitwarden.Bitwarden CallCountingBitwarden where
  unlock _ _ =
    countBackendCall
      (\backendCalls -> backendCalls{unlockCalls = unlockCalls backendCalls + 1})
      unlockResult
  sync _ =
    countBackendCall
      (\backendCalls -> backendCalls{syncCalls = syncCalls backendCalls + 1})
      syncResult
  listItems _ =
    countBackendCall
      (\backendCalls -> backendCalls{listItemsCalls = listItemsCalls backendCalls + 1})
      listItemsResult
  getPassword _ _ =
    countBackendCall
      (\backendCalls -> backendCalls{getPasswordCalls = getPasswordCalls backendCalls + 1})
      getPasswordResult

countBackendCall ::
  (BackendCalls -> BackendCalls) ->
  (MockEnv -> a) ->
  CallCountingBitwarden a
countBackendCall recordCall getResult =
  CallCountingBitwarden $ \mockEnv backendCalls ->
    (getResult mockEnv, recordCall backendCalls)

instance MonadTime CallCountingBitwarden where
  currentTime = CallCountingBitwarden $ \mockEnv backendCalls ->
    (mockCurrentTime mockEnv, backendCalls)
  monotonicTime = pure 0

runCallCountingBitwarden ::
  MockEnv ->
  CallCountingBitwarden a ->
  (a, BackendCalls)
runCallCountingBitwarden mockEnv action =
  runCallCountingBitwardenInternal action mockEnv noBackendCalls
