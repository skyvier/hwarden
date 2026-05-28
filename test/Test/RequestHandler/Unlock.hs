{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.Unlock (tests) where

import Data.Function ((&))
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Helpers
import Test.MockEnv

import qualified Hwarden.Agent as Agent

tests :: TestTree
tests =
  testGroup "unlock"
  [ testProperty "given a locked state, successful unlock action transitions state to unlocked" $
      propertyHandleRequestWithUnlockSuccess
  , testProperty "given a locked state, an unsuccessful unlock operation results in unchanged state, failure response and no effects" $
      propertyHandleRequestWithUnlockFailure
  , testProperty "given an unlocked state, unlock request handling returns unchanged state, \"already unlocked\" response and no effects" $
      propertyHandleRequestWithUnlockedRequestIsIgnored
  , testProperty "a refresh loop effect is only emitted by a successful unlock from the locked state" $
      propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop
  ]

propertyHandleRequestWithUnlockSuccess
  :: Agent.SessionKey
  -> MockEnv
  -> Property
propertyHandleRequestWithUnlockSuccess sessionKey mockEnv =
  let
    request =
      Agent.UnlockRequest
        (Agent.Username "me@example.com")
        (Agent.Password "secret")
    (newState, response, effects) =
      runMockBitwarden
        (mockEnv & withUnlockResult (Right sessionKey))
        (Agent.handleRequestWith request Agent.Locked)
  in
    property $
      case newState of
        Agent.Unlocked unlockedSessionKey _ ->
          unlockedSessionKey == sessionKey
            && response == Agent.successResponse "unlocked"
            && effects == [Agent.StartCacheRefreshLoop sessionKey]
        Agent.Locked ->
          False

propertyHandleRequestWithUnlockFailure
  :: Agent.UnlockError
  -> Agent.Username
  -> Agent.Password
  -> MockEnv
  -> Property
propertyHandleRequestWithUnlockFailure unlockError username password mockEnv =
  let
    request = Agent.UnlockRequest username password
    Agent.Password rawPassword = password
    expectedResponse = expectedFailure password unlockError
    (newState, response, effects) =
      runMockBitwarden
        (mockEnv & withUnlockResult (Left unlockError))
        (Agent.handleRequestWith request Agent.Locked)
  in
    property $
      newState == Agent.Locked
        && response == expectedResponse
        && not
          (encodedResponseContains (T.unpack rawPassword) response)
        && effects == []

propertyHandleRequestWithUnlockedRequestIsIgnored
  :: Agent.SessionKey
  -> Agent.ItemCacheState
  -> Agent.Username
  -> Agent.Password
  -> MockEnv
  -> Property
propertyHandleRequestWithUnlockedRequestIsIgnored
  sessionKey
  cacheState
  username
  password
  mockEnv =
  let
    request = Agent.UnlockRequest username password
    currentState = Agent.Unlocked sessionKey cacheState
    (newState, response, effects) =
      runMockBitwarden
        mockEnv
        (Agent.handleRequestWith request currentState)
  in
    property $
      newState == currentState
        && response == Agent.successResponse "already unlocked"
        && effects == []

propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop ::
  Agent.Request ->
  Agent.AgentState ->
  MockEnv ->
  Property
propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop request initialState mockEnv =
  let
    (newState, _, effects) =
      runMockBitwarden mockEnv (Agent.handleRequestWith request initialState)
  in
    property $
      case effects of
        [] -> True
        [Agent.StartCacheRefreshLoop sessionKey] ->
          case (request, initialState, newState) of
            (Agent.UnlockRequest _ _, Agent.Locked, Agent.Unlocked unlockedSessionKey _) ->
              sessionKey == unlockedSessionKey
            _ ->
              False
        _ -> False

expectedFailure :: Agent.Password -> Agent.UnlockError -> Agent.Response
expectedFailure _ Agent.UnlockUnavailable =
  Agent.failureResponse "bw login failed"
expectedFailure _ Agent.CodeRequired =
  Agent.failureResponse
    "two-factor code required; run scripts/hwarden-first-login"
expectedFailure password (Agent.UnlockFailed err) =
  Agent.failureResponse $
    Agent.PasswordSanitizedFailure $
      Agent.sanitizeUnlockError password err
