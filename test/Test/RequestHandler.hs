{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent

import qualified Test.RequestHandler.Unlock as Unlock
import qualified Test.RequestHandler.Status as Status
import qualified Test.RequestHandler.ListItems as ListItems
import qualified Test.RequestHandler.GetPassword as GetPassword

tests :: TestTree
tests = testGroup "handle request"
  [ Unlock.tests
  , Status.tests
  , ListItems.tests
  , GetPassword.tests

  , testProperty "given any initial state, an unknown request leaves the state unchanged" $
      propertyHandleRequestWithUnknownRequest
  ]

propertyHandleRequestWithUnknownRequest :: Agent.AgentState -> MockEnv -> Property
propertyHandleRequestWithUnknownRequest initialState mockEnv =
  let (newState, response, effects) =
        runMockBitwarden mockEnv (Agent.handleRequestWith Agent.UnknownRequest initialState)
   in property $
        newState == initialState
          && response == Agent.failureResponse "unknown request"
          && effects == []
