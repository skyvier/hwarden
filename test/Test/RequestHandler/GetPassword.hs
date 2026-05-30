{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.GetPassword (tests) where

import Data.Function ((&))
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Sanitize (sanitizeGetPasswordFailure)


tests :: TestTree
tests = testGroup "get-password"
  [ testProperty "given any initial state, get-password preserves state regardless of the backend result" $
      propertyHandleRequestWithGetPasswordPreservesState
  , testProperty "given a locked state, a get-password response indicates that it failed due to locked state" $
      propertyHandleRequestWithGetPasswordLocked
  , testProperty "given an unlocked state, a successful get-password response returns the password as is" $
      propertyHandleRequestWithGetPasswordSuccess
  , testProperty "given an unlocked state, an unavailable get-password backend returns a generic failure" $
      propertyHandleRequestWithGetPasswordUnavailable
  , testProperty "given an unlocked state, a failed get-password backend returns a sanitized failure" $
      propertyHandleRequestWithGetPasswordFailed
  , testProperty "given any initial state, handleGetPassword returns the password and preserves state" $
      propertyHandleGetPasswordSuccess
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

propertyHandleRequestWithGetPasswordSuccess ::
  Agent.SessionKey ->
  Agent.LoginItemId ->
  Agent.PasswordValue ->
  Property
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

propertyHandleRequestWithGetPasswordUnavailable ::
  Agent.SessionKey ->
  Agent.LoginItemId ->
  Property
propertyHandleRequestWithGetPasswordUnavailable sessionKey loginItemId =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withGetPasswordResult (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == Agent.failureResponse "bw get password failed"
          && effects == []

propertyHandleRequestWithGetPasswordFailed ::
  Agent.SessionKey ->
  Agent.LoginItemId ->
  Text ->
  Property
propertyHandleRequestWithGetPasswordFailed sessionKey loginItemId errorText =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      expectedResponse =
        Agent.failureResponse
          (Agent.SessionSanitizedFailure (sanitizeGetPasswordFailure sessionKey errorText))
      (newState, response, effects) =
        runMockBitwarden
          ( defaultMockEnv
              & withGetPasswordResult (Left (Bitwarden.GetPasswordFailed errorText))
          )
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == expectedResponse
          && effects == []

propertyHandleGetPasswordSuccess ::
  Agent.SessionKey ->
  Agent.LoginItemId ->
  Agent.AgentState ->
  Agent.PasswordValue ->
  Property
propertyHandleGetPasswordSuccess sessionKey loginItemId initialState passwordValue =
  let (newState, response, effects) =
        runMockBitwarden
          (defaultMockEnv & withGetPasswordResult (Right passwordValue))
          (Agent.handleGetPassword sessionKey loginItemId initialState)
   in property $
        newState == initialState
          && response == Agent.passwordResultResponse loginItemId passwordValue
          && effects == []
