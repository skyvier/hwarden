{-# LANGUAGE OverloadedStrings #-}

module Test.ExceptionLogging (tests) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), throwIO, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Hwarden.Agent (
  Password (..),
  SessionKey (..),
  Username (..),
  handleConnectionExceptionBoundary,
  handleRefreshIterationExceptionBoundary,
 )
import Hwarden.App (AgentT, Env (..), runAgentT)
import Hwarden.Bitwarden (Bitwarden (unlock))
import qualified Hwarden.Bitwarden as Bitwarden
import Katip (
  ColorStrategy (ColorIfTerminal),
  LogEnv,
  Severity (..),
  Verbosity (V2),
  closeScribes,
  defaultScribeSettings,
  initLogEnv,
  mkHandleScribe,
  permitItem,
  registerScribe,
 )
import System.Directory (
  getTemporaryDirectory,
  removeFile,
 )
import System.FilePath ((</>))
import System.IO (
  Handle,
  hClose,
  hFlush,
  openBinaryTempFile,
 )
import System.Posix.Files (setFileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "exception log sanitization"
    [ testCase "connection boundary does not log exception text" $
        boundaryLogsDoNotExposeSecret
          handleConnectionExceptionBoundary
          "connection-secret"
    , testCase "connection boundary rethrows ThreadKilled" $
        connectionBoundaryRethrowsThreadKilled
    , testCase "Bitwarden command boundary rethrows ThreadKilled" $
        bitwardenCommandBoundaryRethrowsThreadKilled
    , testCase "Bitwarden lock returns timed-out for a hung command" $
        bitwardenLockTimesOut
    , testCase "Bitwarden lock failure logs do not expose the session key" $
        bitwardenLockFailureLogsDoNotExposeSessionKey
    , testCase "refresh iteration boundary does not log exception text" $
        refreshIterationLogsDoNotExposeSecret
          "refresh-secret"
    , testCase "refresh iteration boundary retries after synchronous exceptions" $
        refreshIterationRetriesAfterSynchronousException
    , testCase "refresh iteration boundary rethrows ThreadKilled" $
        refreshIterationRethrowsThreadKilled
    ]

boundaryLogsDoNotExposeSecret ::
  (AgentT () -> AgentT ()) ->
  Text ->
  IO ()
boundaryLogsDoNotExposeSecret boundary secret = do
  logs <-
    withCapturedAgentLogs $ \env ->
      runAgentT env $
        boundary $
          liftSecretException secret
  assertBool
    ("expected logs not to expose secret, got: " <> show logs)
    (not (TE.encodeUtf8 secret `BS.isInfixOf` logs))

connectionBoundaryRethrowsThreadKilled :: IO ()
connectionBoundaryRethrowsThreadKilled = do
  _ <- withCapturedAgentLogs $ \env -> do
    result <-
      try $
        runAgentT env $
          handleConnectionExceptionBoundary $
            liftIO $
              throwIO ThreadKilled
    assertEqual
      "ThreadKilled should escape the connection boundary"
      (Left ThreadKilled)
      (result :: Either AsyncException ())
  pure ()

bitwardenCommandBoundaryRethrowsThreadKilled :: IO ()
bitwardenCommandBoundaryRethrowsThreadKilled = do
  _ <- withSleepingBwEnv $ \env -> do
    currentThread <- myThreadId
    _ <- forkDelayedThreadKilled currentThread
    result <-
      try $
        runAgentT env $
          unlock (Username "me@example.com") (Password "password")
    assertEqual
      "ThreadKilled should escape the Bitwarden command boundary"
      (Left ThreadKilled)
      (result :: Either AsyncException (Either Bitwarden.UnlockError SessionKey))
  pure ()

bitwardenLockTimesOut :: IO ()
bitwardenLockTimesOut = do
  _ <-
    withBwScriptEnv "#!/bin/sh\nsleep 5\n" $ \env -> do
      result <-
        runAgentT env $
          Bitwarden.lock (SessionKey "session-secret")
      assertEqual
        "hung bw lock should time out"
        Bitwarden.LockTimedOut
        result
  pure ()

bitwardenLockFailureLogsDoNotExposeSessionKey :: IO ()
bitwardenLockFailureLogsDoNotExposeSessionKey = do
  let secret = "session-secret"
  logs <-
    withBwScriptEnv "#!/bin/sh\nprintf '%s\\n' \"$BW_SESSION\" 1>&2\nexit 1\n" $ \env -> do
      result <-
        runAgentT env $
          Bitwarden.lock (SessionKey secret)
      assertEqual
        "failed bw lock should be reported as non-fatal failure"
        Bitwarden.LockFailed
        result
  assertBool
    ("expected bw lock logs not to expose secret, got: " <> show logs)
    (not (TE.encodeUtf8 secret `BS.isInfixOf` logs))

refreshIterationLogsDoNotExposeSecret :: Text -> IO ()
refreshIterationLogsDoNotExposeSecret secret = do
  logs <-
    withCapturedAgentLogs $ \env -> do
      shouldContinue <-
        runAgentT env $
          handleRefreshIterationExceptionBoundary $
            liftSecretException secret >> pure False
      assertBool "refresh worker should continue after synchronous exceptions" shouldContinue
  assertBool
    ("expected logs not to expose secret, got: " <> show logs)
    (not (TE.encodeUtf8 secret `BS.isInfixOf` logs))

refreshIterationRetriesAfterSynchronousException :: IO ()
refreshIterationRetriesAfterSynchronousException = do
  _ <- withCapturedAgentLogs $ \env -> do
    shouldContinue <-
      runAgentT env $
        handleRefreshIterationExceptionBoundary $
          liftSecretException "retry-secret" >> pure False
    assertBool "refresh worker should continue after synchronous exceptions" shouldContinue
  pure ()

refreshIterationRethrowsThreadKilled :: IO ()
refreshIterationRethrowsThreadKilled = do
  _ <- withCapturedAgentLogs $ \env -> do
    result <-
      try $
        runAgentT env $
          handleRefreshIterationExceptionBoundary $
            liftIO $
              throwIO ThreadKilled
    assertEqual
      "ThreadKilled should escape the refresh iteration boundary"
      (Left ThreadKilled)
      (result :: Either AsyncException Bool)
  pure ()

liftSecretException :: Text -> AgentT ()
liftSecretException secret =
  liftIO $
    throwIO (userError ("unexpected failure carried " <> show secret))

withCapturedAgentLogs :: (Env -> IO ()) -> IO BS.ByteString
withCapturedAgentLogs action = do
  tempDir <- getTemporaryDirectory
  (logPath, logHandle) <- openBinaryTempFile tempDir "hwarden-agent-log"
  logEnv <- initCapturedLogEnv logHandle
  let env =
        Env
          { envLogEnv = logEnv
          , envLogContexts = mempty
          , envNamespace = "hwarden-agent-test"
          , envBitwardenCliPath = "/bin/false"
          , envBitwardenCliAppDataDir = tempDir
          , envBitwardenServerUrl = Bitwarden.defaultBitwardenServerUrl
          , envCacheRefreshIntervalSeconds = 60
          }
  action env
  _ <- closeScribes logEnv
  hFlush logHandle
  hClose logHandle
  bytes <- BS.readFile logPath
  removeFile logPath
  pure bytes

initCapturedLogEnv :: Handle -> IO LogEnv
initCapturedLogEnv handle = do
  handleScribe <- mkHandleScribe ColorIfTerminal handle (permitItem DebugS) V2
  baseLogEnv <- initLogEnv "hwarden-agent-test" "test"
  registerScribe "capture" handleScribe defaultScribeSettings baseLogEnv

withSleepingBwEnv :: (Env -> IO ()) -> IO BS.ByteString
withSleepingBwEnv action = do
  withBwScriptEnv "#!/bin/sh\nsleep 5\n" action

withBwScriptEnv :: BS.ByteString -> (Env -> IO ()) -> IO BS.ByteString
withBwScriptEnv script action = do
  tempDir <- getTemporaryDirectory
  let bwPath = tempDir </> "hwarden-agent-sleeping-bw"
  BS.writeFile bwPath script
  setFileMode bwPath 0o700
  withCapturedAgentLogs $ \env ->
    action env{envBitwardenCliPath = bwPath}

forkDelayedThreadKilled :: ThreadId -> IO ThreadId
forkDelayedThreadKilled targetThread =
  forkIO $ do
    threadDelay 10000
    throwTo targetThread ThreadKilled
