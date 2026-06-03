{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Sanitization (tests) where

import qualified Data.Text as T
import Data.Data
import Data.Functor.Identity (Identity (runIdentity))

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.Time (MonadTime (currentTime, monotonicTime))
import Control.Monad (forM_)

import GHC.TypeLits

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Logging

import qualified Test.Sanitization.Json as Json
import qualified Test.Sanitization.Show as Show

tests :: TestTree
tests = testGroup "sanitization"
  [ Json.tests
  , Show.tests
  , testGroup "request-log-sanitization"
      [ testProperty "unlock logs do not expose adversarial backend secrets" 
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
  ]

propertyUnlockLogsDoNotExposeSecrets
  :: LeakingMockEnv "session-key" "password-value"
  -> AgentStateWithSessionKey "session-key"
  -> Property
propertyUnlockLogsDoNotExposeSecrets =
  let
    request = 
      Agent.UnlockRequest 
        (Agent.Username "me@example.com") 
        (Agent.Password "password-value")
  in 
    propertyRequestLogsDoNotExposeSecrets request

propertyRequestLogsDoNotExposeSecrets
  :: Agent.Request
  -> LeakingMockEnv "session-key" "password-value"
  -> AgentStateWithSessionKey "session-key"
  -> Property
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
        ] logs


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
        (logInfoF @"password result was: %{PasswordValue}" password
          :: LoggingMockBitwarden ())
        return $ Right password

instance MonadTime LoggingMockBitwarden where
  currentTime =
    LoggingMockBitwarden (asks mockCurrentTime)
  monotonicTime =
    pure 0

newtype AgentStateWithSessionKey (sessionKey :: Symbol) = 
  AgentStateWithSessionKey Agent.AgentState
  deriving newtype (Show, Eq)

instance KnownSymbol sessionKey => Arbitrary (AgentStateWithSessionKey sessionKey) where
  arbitrary = do
    let sessionKeyText = T.pack $ symbolVal (Proxy @sessionKey)
    unlockedSession <- Agent.Unlocked (Agent.SessionKey sessionKeyText) <$> arbitrary
    AgentStateWithSessionKey <$> elements
      [ Agent.Locked, unlockedSession ]

newtype LeakingMockEnv (sessionKey :: Symbol) (password :: Symbol) = LeakingMockEnv MockEnv
  deriving newtype Show

instance (KnownSymbol sessionKey, KnownSymbol password) => Arbitrary (LeakingMockEnv sessionKey password) where 
  arbitrary = do
    let 
      sessionKeyText = T.pack $ symbolVal (Proxy @sessionKey)
      passwordText = T.pack $ symbolVal (Proxy @password)
      mockCurrentTime = mockNow

    unlockResult <- elements 
      [ Left Agent.UnlockUnavailable
      , Left (Agent.UnlockFailed passwordText)
      , Right (Agent.SessionKey passwordText)
      ]
    listItemsResult <- elements
      [ Left Agent.ListItemsUnavailable
      , Left (Agent.ListItemsFailed sessionKeyText)
      , Right []
      ]
    syncResult <- elements
      [ Left Bitwarden.SyncUnavailable
      , Left (Bitwarden.SyncFailed sessionKeyText)
      , Right ()
      ]
    getPasswordResult <- elements
      [ Left Bitwarden.GetPasswordUnavailable 
      , Left (Bitwarden.GetPasswordFailed sessionKeyText)
      , Right (Agent.PasswordValue passwordText)
      ]
    return $ LeakingMockEnv $
      MockEnv {..}
