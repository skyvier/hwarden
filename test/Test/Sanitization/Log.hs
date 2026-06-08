{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sanitization.Log (tests) where

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Time (MonadTime (currentTime, monotonicTime))
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Data
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Text as T
import GHC.TypeLits
import Hwarden.Agent (handleShutdownCleanupLoggedWith)
import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Logging
import Test.MockEnv
import Test.Sanitization.LeakingMockEnv
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "request-log-sanitization"
    [ testGroup
        "handleRequestWith"
        [ testProperty "unlock logs do not expose adversarial backend secrets" $
            propertyUnlockLogsDoNotExposeSecrets
        , testProperty "status logs do not expose adversarial backend secrets" $
            propertyRequestLogsDoNotExposeSecrets Agent.Status
        , testProperty "list-items logs do not expose adversarial backend secrets" $
            propertyRequestLogsDoNotExposeSecrets Agent.ListItems
        , testProperty "get-password logs do not expose adversarial backend secrets" $
            propertyRequestLogsDoNotExposeSecrets $
              Agent.GetPasswordRequest (Agent.LoginItemId "login-item-id")
        , testProperty "unknown request logs do not expose the current session key" $
            propertyRequestLogsDoNotExposeSecrets Agent.UnknownRequest
        ]
    , testGroup
        "handleShutdownCleanupLoggedWith"
        [ testProperty "logs do not expose adversarial backend secrets" $
            propertyShutdownLockingDoesNotExposeSecrets
        ]
    ]

propertyShutdownLockingDoesNotExposeSecrets ::
  LeakingMockEnv "session-key" "password-value" ->
  AgentStateWithSessionKey "session-key" ->
  Property
propertyShutdownLockingDoesNotExposeSecrets
  (LeakingMockEnv mockEnv)
  (AgentStateWithSessionKey agentState) =
    let
      fakeModifyMvar updateState = snd <$> updateState agentState

      (_, logs) =
        runLoggingMockBitwarden
          mockEnv
          (handleShutdownCleanupLoggedWith fakeModifyMvar)
     in
      counterexample (show logs) $
        assertLogsDoNotExposeSecrets
          [ "session-key"
          , "password-value"
          ]
          logs

propertyUnlockLogsDoNotExposeSecrets ::
  LeakingMockEnv "session-key" "password-value" ->
  AgentStateWithSessionKey "session-key" ->
  Property
propertyUnlockLogsDoNotExposeSecrets =
  let
    request =
      Agent.UnlockRequest
        (Agent.Username "me@example.com")
        (Agent.Password "password-value")
   in
    propertyRequestLogsDoNotExposeSecrets request

propertyRequestLogsDoNotExposeSecrets ::
  Agent.Request ->
  LeakingMockEnv "session-key" "password-value" ->
  AgentStateWithSessionKey "session-key" ->
  Property
propertyRequestLogsDoNotExposeSecrets
  req
  (LeakingMockEnv mockEnv)
  (AgentStateWithSessionKey agentState) =
    let
      (_, logs) =
        runLoggingMockBitwarden
          mockEnv
          (loggedHandleRequestWith req agentState)
     in
      counterexample (show logs) $
        assertLogsDoNotExposeSecrets
          [ "session-key"
          , "password-value"
          ]
          logs

assertLogsDoNotExposeSecrets :: [T.Text] -> [T.Text] -> Bool
assertLogsDoNotExposeSecrets secrets logs =
  all (\secret -> not (any (secret `T.isInfixOf`) logs)) secrets

loggedHandleRequestWith ::
  Agent.Request ->
  Agent.AgentState ->
  LoggingMockBitwarden (Agent.AgentState, Agent.Response, [Agent.Effect])
loggedHandleRequestWith request initialState = do
  (logInfoF @"received request: %{Request}" request :: LoggingMockBitwarden ())
  result@(_, response, effects) <- Agent.handleRequestWith request initialState
  (logInfoF @"sent response: %{Response}" response :: LoggingMockBitwarden ())

  forM_ effects $ \effect ->
    (logInfoF @"effects: %{effect}" (field @"effect" effect) :: LoggingMockBitwarden ())

  pure result

newtype LoggingMockBitwarden a = LoggingMockBitwarden
  { runLoggingMockBitwardenInternal :: ReaderT MockEnv (WriterT [T.Text] Identity) a
  }
  deriving newtype (Functor, Applicative, Monad)

instance MonadLog LoggingMockBitwarden where
  unsafeLogInfo msg = recordLog (renderLogMessage msg)

runLoggingMockBitwarden :: MockEnv -> LoggingMockBitwarden a -> (a, [T.Text])
runLoggingMockBitwarden mockEnv action =
  runIdentity $
    runWriterT $
      runReaderT (runLoggingMockBitwardenInternal action) mockEnv

recordLog :: T.Text -> LoggingMockBitwarden ()
recordLog message =
  LoggingMockBitwarden (tell [message])

instance Bitwarden.Bitwarden LoggingMockBitwarden where
  unlock username password = do
    (logInfoF @"running bw login: %{Username} with %{Password}" username password :: LoggingMockBitwarden ())
    LoggingMockBitwarden (asks unlockResult)
  listItems sessionKey = do
    (logInfoF @"running bw list items with %{SessionKey}" sessionKey :: LoggingMockBitwarden ())
    LoggingMockBitwarden (asks listItemsResult)
  sync sessionKey = do
    (logInfoF @"running bw sync with %{SessionKey}" sessionKey :: LoggingMockBitwarden ())
    LoggingMockBitwarden (asks syncResult)
  getPassword sessionKey loginItemId = do
    (logInfoF @"running bw get password for %{LoginItemId} with %{SessionKey}" loginItemId sessionKey :: LoggingMockBitwarden ())
    passwordResult <- LoggingMockBitwarden (asks getPasswordResult)
    case passwordResult of
      Left err -> do
        (logInfoS @"got an error" :: LoggingMockBitwarden ())
        return $ Left err
      Right password -> do
        (logInfoF @"password result was: %{PasswordValue}" password :: LoggingMockBitwarden ())
        return $ Right password
  lock sessionKey = do
    (logInfoF @"running bw lock with %{SessionKey}" sessionKey :: LoggingMockBitwarden ())
    pure Bitwarden.LockSucceeded

instance MonadTime LoggingMockBitwarden where
  currentTime =
    LoggingMockBitwarden (asks mockCurrentTime)
  monotonicTime =
    pure 0

newtype AgentStateWithSessionKey (sessionKey :: Symbol)
  = AgentStateWithSessionKey Agent.AgentState
  deriving newtype (Show, Eq)

instance (KnownSymbol sessionKey) => Arbitrary (AgentStateWithSessionKey sessionKey) where
  arbitrary = do
    let sessionKeyText = T.pack $ symbolVal (Proxy @sessionKey)
    unlockedSession <- Agent.Unlocked (Agent.SessionKey sessionKeyText) <$> arbitrary
    AgentStateWithSessionKey
      <$> elements
        [ Agent.Locked
        , unlockedSession
        ]
