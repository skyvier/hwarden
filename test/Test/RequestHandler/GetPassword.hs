{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.GetPassword (tests) where

import Data.Function ((&))
  
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden


tests :: TestTree
tests = testGroup "get-password"
  [ testProperty "given any initial state, get-password preserves state regardless of the backend result" $
      propertyHandleRequestWithGetPasswordPreservesState
  , testProperty "given a locked state, a get-password response indicates that it failed due to locked state" $
      propertyHandleRequestWithGetPasswordLocked
  , testProperty "given an unlocked state, a successful get-password response returns the password as is" $
      propertyHandleRequestWithGetPasswordSuccess
  ]

propertyHandleRequestWithGetPasswordPreservesState ::
  Agent.AgentState ->
  Agent.LoginItemId ->
  Either Bitwarden.GetPasswordError Agent.PasswordValue ->
  Property
propertyHandleRequestWithGetPasswordPreservesState initialState loginItemId mockGetPasswordResult =
  let (newState, _, effects) =
        runMockBitwarden
          (defaultMockEnv & withGetPasswordResult mockGetPasswordResult)
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) initialState)
   in property (newState == initialState && effects == [])


propertyHandleRequestWithGetPasswordLocked :: Agent.LoginItemId -> Property
propertyHandleRequestWithGetPasswordLocked loginItemId =
  let currentState = Agent.Locked
      (newState, response, effects) =
        runMockBitwarden
          defaultMockEnv
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == Agent.failureResponse "locked"
          && effects == []

propertyHandleRequestWithGetPasswordSuccess :: Agent.SessionKey -> Agent.LoginItemId -> Agent.PasswordValue -> Property
propertyHandleRequestWithGetPasswordSuccess sessionKey loginItemId passwordValue =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withGetPasswordResult (Right passwordValue))
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == Agent.passwordResultResponse loginItemId passwordValue
          && effects == []
