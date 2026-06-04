{-# LANGUAGE OverloadedStrings #-}

module Test.RequestHandler.Status (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent

tests :: TestTree
tests =
  testGroup
    "status"
    [ testCase "given a locked state, a status request returns locked" $
        let currentState = Agent.Locked
            (newState, response, effects) =
              runMockBitwarden
                defaultMockEnv
                (Agent.handleRequestWith Agent.Status currentState)
         in do
              newState @?= currentState
              response @?= Agent.successResponse "locked"
              effects @?= []
    , testProperty "given an unlocked state, a status request returns unlocked" $ \sessionKey ->
        let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
            (newState, response, effects) =
              runMockBitwarden
                defaultMockEnv
                (Agent.handleRequestWith Agent.Status currentState)
         in property $
              newState == currentState
                && response == Agent.successResponse "unlocked"
                && effects == []
    ]
