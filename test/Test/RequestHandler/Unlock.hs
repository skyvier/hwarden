{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.Unlock (tests) where

import Data.Function ((&))
  
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent

tests :: TestTree
tests= testGroup "unlock"
  [ testProperty "given a locked state, successful unlock action transitions state to unlocked" $
      propertyHandleRequestWithUnlockSuccess
  , testProperty "given a locked state, a successful unlock with a failed initial item cache fill still returns unlocked and records the cache failure" $
      propertyHandleRequestWithUnlockCacheFillFailure
  , testProperty "given an unlocked state, an unlock action leaves the state unchanged regardless of the result of the unlock action" $
      propertyHandleRequestWithUnlockedIgnoresUnlockResult
  , testProperty "a refresh loop effect is only emitted by a successful unlock from the locked state" $
      propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop

  -- XXX: handleUnlock instad of handleRequestWith
  , testProperty "given a locked state, a failed unlock action leaves the state unchanged" $
      propertyHandleUnlockFailure
  , testProperty "given a locked state, successful unlock action transitions state to unlocked" $
      propertyHandleUnlockSuccess
  , testProperty "given a successful unlock and a failed initial item cache fill, unlock still succeeds and records the cache failure" $
      propertyHandleUnlockCacheFillFailure
  ]

propertyHandleRequestWithUnlockSuccess :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleRequestWithUnlockSuccess sessionKey items =
  let (newState, response, effects) =
        runMockBitwarden
          ( defaultMockEnv
              & withUnlockResult (Right sessionKey)
              & withListItemsResult (Right items)
              & withSyncResult (Right ())
          )
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState
          == Agent.Unlocked
            sessionKey
            (Agent.CacheReady (Agent.CacheEntry items mockNow) Agent.LatestRefreshSucceeded)
          && response == Agent.successResponse "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyHandleRequestWithUnlockCacheFillFailure ::
  Agent.SessionKey ->
  Agent.ListItemsError ->
  Property
propertyHandleRequestWithUnlockCacheFillFailure sessionKey listItemsFailure =
  let expectedCacheFailure =
        Agent.cacheFillFailureFromListItemsError sessionKey listItemsFailure
      (newState, response, effects) =
        runMockBitwarden
          ( defaultMockEnv
              & withUnlockResult (Right sessionKey)
              & withListItemsResult (Left listItemsFailure)
              & withSyncResult (Right ())
          )
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState == Agent.Unlocked sessionKey (Agent.CacheFillError expectedCacheFailure)
          && response == Agent.successResponse "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyHandleRequestWithUnlockedIgnoresUnlockResult 
  :: Agent.SessionKey 
  -> Agent.ItemCacheState
  -> MockEnv 
  -> Property
propertyHandleRequestWithUnlockedIgnoresUnlockResult sessionKey cacheState mockEnv =
  let currentState = Agent.Unlocked sessionKey cacheState 
      (newState, response, effects) =
        runMockBitwarden
          mockEnv
          (Agent.handleRequestWith 
            (Agent.UnlockRequest 
              (Agent.Username "me@example.com") 
              (Agent.Password "secret")
            ) currentState)
   in property $
        newState == currentState
          && response == Agent.successResponse "already unlocked"
          && effects == []

propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop ::
  Agent.Request ->
  Agent.AgentState ->
  MockEnv ->
  Property
propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop request initialState mockEnv =
  let (newState, _, effects) =
        runMockBitwarden mockEnv (Agent.handleRequestWith request initialState)
   in property $
        case effects of
          [] -> True
          [Agent.StartCacheRefreshLoop sessionKey] ->
            case (request, initialState, newState) of
              (Agent.UnlockRequest _ _, Agent.Locked, Agent.Unlocked unlockedSessionKey _) ->
                sessionKey == unlockedSessionKey
              _ ->
                False
          _ -> False

propertyHandleUnlockSuccess :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleUnlockSuccess sessionKey items =
  let (newState, response, effects) =
        runMockBitwarden
          ( defaultMockEnv
              & withUnlockResult (Right sessionKey)
              & withListItemsResult (Right items)
              & withSyncResult (Right ())
          )
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState
          == Agent.Unlocked
            sessionKey
            (Agent.CacheReady (Agent.CacheEntry items mockNow) Agent.LatestRefreshSucceeded)
          && response == Agent.successResponse "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyHandleUnlockFailure :: Agent.UnlockError -> Property
propertyHandleUnlockFailure unlockError =
  let (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withUnlockResult (Left unlockError))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Locked
          && response == expectedFailure unlockError
          && not (encodedResponseContains "secret" response)
          && effects == []

propertyHandleUnlockCacheFillFailure ::
  Agent.SessionKey ->
  Agent.ListItemsError ->
  Property
propertyHandleUnlockCacheFillFailure sessionKey listItemsFailure =
  let expectedCacheFailure =
        Agent.cacheFillFailureFromListItemsError sessionKey listItemsFailure
      (newState, response, effects) =
        runMockBitwarden
          ( defaultMockEnv
              & withUnlockResult (Right sessionKey)
              & withListItemsResult (Left listItemsFailure)
              & withSyncResult (Right ())
          )
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Unlocked sessionKey (Agent.CacheFillError expectedCacheFailure)
          && response == Agent.successResponse "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

expectedFailure :: Agent.UnlockError -> Agent.Response
expectedFailure Agent.UnlockUnavailable = Agent.failureResponse "bw login failed"
expectedFailure Agent.CodeRequired = Agent.failureResponse "two-factor code required; run scripts/hwarden-first-login"
expectedFailure (Agent.UnlockFailed err) =
  Agent.failureResponse (Agent.PasswordSanitizedFailure (Agent.sanitizeUnlockError (Agent.Password "secret") err))
