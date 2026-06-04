{-# LANGUAGE OverloadedStrings #-}

module Test.Agent.Decide (tests) where

import qualified Hwarden.Agent as Agent
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "decide"
    [ testCase "given a locked state, an unlock request triggers an unlock decision" $
        Agent.decide
          (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret"))
          Agent.Locked
          @?= Agent.UnlockAction (Agent.Username "me@example.com") (Agent.Password "secret")
    , testCase "given a locked state, a status request replies locked" $
        Agent.decide Agent.Status Agent.Locked
          @?= Agent.Reply (Agent.successResponse "locked")
    , testCase "given a locked state, a list-items request replies locked failure" $
        Agent.decide Agent.ListItems Agent.Locked
          @?= Agent.Reply (Agent.failureResponse "locked")
    , testProperty "given a locked state, a get-password request replies locked failure" $
        propertyDecideGetPasswordLocked
    , testCase "given an unlocked state, an unlock request replies already unlocked" $
        Agent.decide
          (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret"))
          (Agent.Unlocked (Agent.SessionKey "session-key") Agent.CacheNotYetFilled)
          @?= Agent.Reply (Agent.successResponse "already unlocked")
    , testProperty "given an unlocked state, a status request replies unlocked" $
        propertyDecideStatusUnlocked
    , testProperty "given an unlocked state with cached items, a list-items request triggers a cached list action" $
        propertyDecideListItemsUnlocked
    , testCase "given an unlocked state with a not-yet-filled cache, a list-items request replies with cache unavailable" $
        Agent.decide
          Agent.ListItems
          (Agent.Unlocked (Agent.SessionKey "session-key") Agent.CacheNotYetFilled)
          @?= Agent.Reply (Agent.failureResponse "item cache unavailable")
    , testProperty "given an unlocked state with a failed cache fill, a list-items request replies with cache unavailable" $
        propertyDecideListItemsFailedCacheFill
    , testProperty "given an unlocked state, a get-password request triggers password retrieval for the requested id" $
        propertyDecideGetPasswordUnlocked
    , testCase "given any state, an unknown request replies with failure" $
        Agent.decide Agent.UnknownRequest Agent.Locked
          @?= Agent.Reply (Agent.failureResponse "unknown request")
    ]

propertyDecideGetPasswordLocked :: Agent.LoginItemId -> Property
propertyDecideGetPasswordLocked loginItemId =
  property $
    Agent.decide (Agent.GetPasswordRequest loginItemId) Agent.Locked
      == Agent.Reply (Agent.failureResponse "locked")

propertyDecideStatusUnlocked :: Agent.SessionKey -> Property
propertyDecideStatusUnlocked sessionKey =
  property $
    Agent.decide Agent.Status (Agent.Unlocked sessionKey Agent.CacheNotYetFilled)
      == Agent.Reply (Agent.successResponse "unlocked")

propertyDecideListItemsUnlocked :: Agent.SessionKey -> Agent.CacheEntry -> Agent.LatestRefreshStatus -> Property
propertyDecideListItemsUnlocked sessionKey cacheEntry latestRefreshStatus =
  property $
    Agent.decide
      Agent.ListItems
      (Agent.Unlocked sessionKey (Agent.CacheReady cacheEntry latestRefreshStatus))
      == Agent.ListItemsAction cacheEntry latestRefreshStatus

propertyDecideListItemsFailedCacheFill :: Agent.SessionKey -> Agent.CacheFillFailure -> Property
propertyDecideListItemsFailedCacheFill sessionKey cacheFillFailure =
  property $
    Agent.decide
      Agent.ListItems
      (Agent.Unlocked sessionKey (Agent.CacheFillError cacheFillFailure))
      == Agent.Reply (Agent.failureResponse "item cache unavailable")

propertyDecideGetPasswordUnlocked :: Agent.SessionKey -> Agent.LoginItemId -> Property
propertyDecideGetPasswordUnlocked sessionKey loginItemId =
  property $
    Agent.decide (Agent.GetPasswordRequest loginItemId) (Agent.Unlocked sessionKey Agent.CacheNotYetFilled)
      == Agent.GetPasswordAction sessionKey loginItemId
