{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.Status (tests) where
  
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent


tests :: TestTree 
tests = testGroup "status"
  [ testProperty "given any state, a status request reports the lock state and preserves state" $
      propertyHandleRequestWithStatusReportsLockStateAndPreservesState
  , testProperty "given an unlocked state, a status request ignores the item cache state" $
      propertyHandleRequestWithStatusIgnoresUnlockedCacheState

  ]

propertyHandleRequestWithStatusReportsLockStateAndPreservesState ::
  Agent.AgentState ->
  Property
propertyHandleRequestWithStatusReportsLockStateAndPreservesState currentState =
  let (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.Status currentState)
      expectedResponse =
        case currentState of
          Agent.Locked ->
            Agent.successResponse "locked"
          Agent.Unlocked _ _ ->
            Agent.successResponse "unlocked"
   in property $
        newState == currentState
          && response == expectedResponse
          && effects == []

propertyHandleRequestWithStatusIgnoresUnlockedCacheState ::
  Agent.SessionKey ->
  Agent.ItemCacheState ->
  Property
propertyHandleRequestWithStatusIgnoresUnlockedCacheState sessionKey cacheState =
  let currentState = Agent.Unlocked sessionKey cacheState
      (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith Agent.Status currentState)
   in property $
        newState == currentState
          && response == Agent.successResponse "unlocked"
          && effects == []
