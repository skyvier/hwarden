{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shutdown (tests) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Hwarden.Agent (
  AgentState (..),
  ItemCacheState (CacheNotYetFilled),
  LockResult (..),
  SessionKey (..),
  ShutdownLockOutcome (..),
  finallyAll,
  handleShutdownCleanupWith,
 )
import Hwarden.Bitwarden (Bitwarden (..), GetPasswordError (GetPasswordUnavailable), ListItemsError (ListItemsUnavailable), SyncError (SyncUnavailable), UnlockError (UnlockUnavailable))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "shutdown cleanup"
    [ testGroup
        "handleShutdownCleanupWith"
        [ testCase "no live session skips bw lock" $
            let (outcome, finalState, lockAttempts) =
                  runShutdownMock LockSucceeded Locked $
                    handleShutdownCleanupWith shutdownStateUpdate
             in do
                  outcome @?= ShutdownLockSkippedNoLiveSession
                  finalState @?= Locked
                  lockAttempts @?= []
        , testCase "live session attempts bw lock" $
            let sessionKey = SessionKey "session-secret"
                (outcome, finalState, lockAttempts) =
                  runShutdownMock LockSucceeded (Unlocked sessionKey CacheNotYetFilled) $
                    handleShutdownCleanupWith shutdownStateUpdate
             in do
                  outcome @?= ShutdownLockSucceeded
                  finalState @?= Locked
                  lockAttempts @?= [sessionKey]
        , testCase "lock command failure still leaves shutdown cleanup successful" $
            let sessionKey = SessionKey "session-secret"
                (outcome, finalState, lockAttempts) =
                  runShutdownMock LockFailed (Unlocked sessionKey CacheNotYetFilled) $
                    handleShutdownCleanupWith shutdownStateUpdate
             in do
                  outcome @?= ShutdownLockFailed
                  finalState @?= Locked
                  lockAttempts @?= [sessionKey]
        , testCase "lock timeout still leaves shutdown cleanup successful" $
            let sessionKey = SessionKey "session-secret"
                (outcome, finalState, lockAttempts) =
                  runShutdownMock LockTimedOut (Unlocked sessionKey CacheNotYetFilled) $
                    handleShutdownCleanupWith shutdownStateUpdate
             in do
                  outcome @?= ShutdownLockTimedOut
                  finalState @?= Locked
                  lockAttempts @?= [sessionKey]
        , testCase "repeated shutdown cleanup attempts bw lock at most once" $
            let sessionKey = SessionKey "session-secret"
                (outcomes, finalState, lockAttempts) =
                  runShutdownMock LockSucceeded (Unlocked sessionKey CacheNotYetFilled) $ do
                    firstOutcome <- handleShutdownCleanupWith shutdownStateUpdate
                    secondOutcome <- handleShutdownCleanupWith shutdownStateUpdate
                    pure [firstOutcome, secondOutcome]
             in do
                  outcomes @?= [ShutdownLockSucceeded, ShutdownLockSkippedNoLiveSession]
                  finalState @?= Locked
                  lockAttempts @?= [sessionKey]
        ]
    , testGroup
        "finallyAll"
        [ testCase "finallyAll runs cleanups in order after success" $ do
            events <- newIORef []
            result <-
              ( finallyAll
                  (recordEvent events "action" >> pure "result")
                  [ recordEvent events "cleanup-1"
                  , recordEvent events "cleanup-2"
                  , recordEvent events "cleanup-3"
                  ] ::
                  IO String
              )
            result @?= "result"
            readIORef events
              >>= (@?= ["action", "cleanup-1", "cleanup-2", "cleanup-3"])
        , testCase "finallyAll runs cleanups after action failure" $ do
            events <- newIORef []
            result <-
              try @SomeException $
                finallyAll
                  (recordEvent events "action" >> fail "action failed")
                  [ recordEvent events "cleanup-1"
                  , recordEvent events "cleanup-2"
                  , recordEvent events "cleanup-3"
                  ]
            assertException result
            readIORef events
              >>= (@?= ["action", "cleanup-1", "cleanup-2", "cleanup-3"])
        , testCase "finallyAll continues cleanups after cleanup failure" $ do
            events <- newIORef []
            result <-
              try @SomeException $
                finallyAll
                  (recordEvent events "action")
                  [ recordEvent events "cleanup-1" >> fail "cleanup failed"
                  , recordEvent events "cleanup-2"
                  , recordEvent events "cleanup-3"
                  ]
            assertException result
            readIORef events
              >>= (@?= ["action", "cleanup-1", "cleanup-2", "cleanup-3"])
        ]
    ]

recordEvent :: IORef [String] -> String -> IO ()
recordEvent events event =
  modifyIORef' events (<> [event])

assertException :: Either SomeException a -> IO ()
assertException result =
  assertBool "expected finallyAll to throw" $
    case result of
      Left _ -> True
      Right _ -> False

newtype ShutdownMock a = ShutdownMock
  { runShutdownMockInternal :: ReaderT LockResult (State ShutdownMockState) a
  }
  deriving (Functor, Applicative, Monad)

data ShutdownMockState = ShutdownMockState
  { shutdownLockAttempts :: [SessionKey]
  , shutdownAgentState :: AgentState
  }

runShutdownMock :: LockResult -> AgentState -> ShutdownMock a -> (a, AgentState, [SessionKey])
runShutdownMock lockResult initialAgentState action =
  let (result, finalMockState) =
        runState
          (runReaderT (runShutdownMockInternal action) lockResult)
          ShutdownMockState
            { shutdownLockAttempts = []
            , shutdownAgentState = initialAgentState
            }
   in (result, shutdownAgentState finalMockState, shutdownLockAttempts finalMockState)

shutdownStateUpdate ::
  (AgentState -> ShutdownMock (AgentState, Maybe SessionKey)) ->
  ShutdownMock (Maybe SessionKey)
shutdownStateUpdate updateAgentState = ShutdownMock $ do
  agentState <- gets shutdownAgentState
  (newAgentState, maybeSessionKey) <-
    runShutdownMockInternal (updateAgentState agentState)
  modify $ \mockState ->
    mockState{shutdownAgentState = newAgentState}
  pure maybeSessionKey

instance Bitwarden ShutdownMock where
  unlock _ _ = pure (Left UnlockUnavailable)
  listItems _ = pure (Left ListItemsUnavailable)
  sync _ = pure (Left SyncUnavailable)
  getPassword _ _ = pure (Left GetPasswordUnavailable)
  lock sessionKey = ShutdownMock $ do
    modify $ \mockState ->
      mockState{shutdownLockAttempts = shutdownLockAttempts mockState <> [sessionKey]}
    ask
