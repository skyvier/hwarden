{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shutdown (tests) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (State, modify, runState)
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Hwarden.Agent (
  AgentState (..),
  ItemCacheState (CacheNotYetFilled),
  LockResult (..),
  SessionKey (..),
  ShutdownLockOutcome (..),
  finallyAll,
  handleShutdownCleanupWith,
  logShutdownLockOutcome,
 )
import Hwarden.Bitwarden (Bitwarden (..), GetPasswordError (GetPasswordUnavailable), ListItemsError (ListItemsUnavailable), SyncError (SyncUnavailable), UnlockError (UnlockUnavailable))
import Hwarden.Logging (MonadLog (..), renderLogMessage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "shutdown cleanup"
    [ testCase "no live session skips bw lock" $
        let (result, lockAttempts) =
              runShutdownMock LockSucceeded $
                handleShutdownCleanupWith Locked
         in do
              result @?= (Locked, ShutdownLockSkippedNoLiveSession)
              lockAttempts @?= []
    , testCase "live session attempts bw lock" $
        let sessionKey = SessionKey "session-secret"
            (result, lockAttempts) =
              runShutdownMock LockSucceeded $
                handleShutdownCleanupWith (Unlocked sessionKey CacheNotYetFilled)
         in do
              result @?= (Locked, ShutdownLockSucceeded)
              lockAttempts @?= [sessionKey]
    , testCase "lock command failure still leaves shutdown cleanup successful" $
        let sessionKey = SessionKey "session-secret"
            (result, lockAttempts) =
              runShutdownMock LockFailed $
                handleShutdownCleanupWith (Unlocked sessionKey CacheNotYetFilled)
         in do
              result @?= (Locked, ShutdownLockFailed)
              lockAttempts @?= [sessionKey]
    , testCase "lock timeout still leaves shutdown cleanup successful" $
        let sessionKey = SessionKey "session-secret"
            (result, lockAttempts) =
              runShutdownMock LockTimedOut $
                handleShutdownCleanupWith (Unlocked sessionKey CacheNotYetFilled)
         in do
              result @?= (Locked, ShutdownLockTimedOut)
              lockAttempts @?= [sessionKey]
    , testCase "repeated shutdown cleanup attempts bw lock at most once" $
        let sessionKey = SessionKey "session-secret"
            (finalState, lockAttempts) =
              runShutdownMock LockSucceeded $ do
                (firstState, _) <-
                  handleShutdownCleanupWith (Unlocked sessionKey CacheNotYetFilled)
                (secondState, _) <- handleShutdownCleanupWith firstState
                pure secondState
         in do
              finalState @?= Locked
              lockAttempts @?= [sessionKey]
    , testCase "shutdown logs do not contain the session key" $ do
        let rawSessionKey = "session-secret"
        logs <-
          execShutdownLogs $ do
            logShutdownLockOutcome ShutdownLockSucceeded
            logShutdownLockOutcome ShutdownLockFailed
            logShutdownLockOutcome ShutdownLockTimedOut
            logShutdownLockOutcome ShutdownLockSkippedNoLiveSession
        assertBool
          ("expected shutdown logs not to expose session key, got: " <> show logs)
          (not (any (rawSessionKey `T.isInfixOf`) logs))
    , testCase "finallyAll runs cleanups in order after success" $ do
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
  { runShutdownMockInternal :: ReaderT LockResult (State [SessionKey]) a
  }
  deriving (Functor, Applicative, Monad)

runShutdownMock :: LockResult -> ShutdownMock a -> (a, [SessionKey])
runShutdownMock lockResult action =
  runState (runReaderT (runShutdownMockInternal action) lockResult) []

instance Bitwarden ShutdownMock where
  unlock _ _ = pure (Left UnlockUnavailable)
  listItems _ = pure (Left ListItemsUnavailable)
  sync _ = pure (Left SyncUnavailable)
  getPassword _ _ = pure (Left GetPasswordUnavailable)
  lock sessionKey = ShutdownMock $ do
    modify (<> [sessionKey])
    ask

newtype ShutdownLog a = ShutdownLog
  { runShutdownLog :: WriterT [T.Text] IO a
  }
  deriving (Functor, Applicative, Monad)

instance MonadLog ShutdownLog where
  unsafeLogInfo message =
    ShutdownLog (tell [renderLogMessage message])

execShutdownLogs :: ShutdownLog () -> IO [T.Text]
execShutdownLogs =
  execWriterT . runShutdownLog
