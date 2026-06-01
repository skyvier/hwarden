{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Sanitization (tests) where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.Time (MonadTime (currentTime, monotonicTime))
import qualified Data.Text as T
import Data.Data
import Data.Function ((&))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import Test.MockEnv
import Test.Helpers

import GHC.TypeLits

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden

-- TODO: remember to test that all requests received by a locked agent 
-- are responded to with "locked"

tests :: TestTree
tests = testGroup "secret redaction invariants"
  [ testProperty "given any state, the encoded status response never exposes the session key" $
      propertyHandleRequestWithStatusDoesNotExposeSessionKey
  , testProperty "given any state, an unknown command never exposes the session key" $
      propertyHandleRequestWithUnknownDoesNotExposeSessionKey
  , testProperty "given an unlocked state, the encoded list-items response never exposes the session key" $
      propertyHandleRequestWithListItemsDoesNotExposeSessionKey
  , testProperty "given an unlocked state, the encoded get-password response never exposes the session key" $
      propertyHandleRequestWithGetPasswordDoesNotExposeSessionKey
  , testProperty "given an unlocked state, bitwarden CLI outputs are always redacted of secrets" $
      propertyHandleRequestWithDoesNotExposeSecrets
  , testGroup "request-log-sanitization"
      [ testCase "unlock logs do not expose an adversarial backend secret" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            adversarialEnv
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password adversarialSecret))
            Agent.Locked
      , testCase "successful unlock logs do not expose the new session key" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            ( adversarialEnv
                & withUnlockResult (Right (Agent.SessionKey adversarialSecret))
                & withSyncResult (Left (Bitwarden.SyncFailed adversarialSecret))
            )
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "safe-password"))
            Agent.Locked
      , testCase "status logs do not expose the current session key" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            adversarialEnv
            Agent.Status
            adversarialUnlockedState
      , testCase "list-items logs do not expose the current session key" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            adversarialEnv
            Agent.ListItems
            adversarialUnlockedState
      , testCase "get-password failure logs do not expose backend failure text" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            adversarialEnv
            (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            adversarialUnlockedState
      , testCase "get-password success logs do not expose the returned password" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            (adversarialEnv & withGetPasswordResult (Right (Agent.PasswordValue adversarialSecret)))
            (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            adversarialUnlockedState
      , testCase "unknown request logs do not expose the current session key" $
          assertLogsDoNotExposeSecret
            adversarialSecret
            adversarialEnv
            Agent.UnknownRequest
            adversarialUnlockedState
      ]
  -- TODO: check that even if 'bw list items' includes secrets in some of the 
  -- attributes, those are not included in the response
  
  -- XXX: This does not belong to this testGroup
  , testProperty "given a successful get-password response, show never exposes the plaintext password" $
      propertyPasswordResultShowDoesNotExposePassword
  ]

propertyPasswordResultShowDoesNotExposePassword :: Agent.LoginItemId -> String -> Property
propertyPasswordResultShowDoesNotExposePassword loginItemId passwordText =
  not (null passwordText) ==>
    property
      ( let passwordNeedle =
              T.pack ("pw-needle-" <> passwordText <> "-end")
            response =
              Agent.passwordResultResponse
                loginItemId
                (Agent.PasswordValue passwordNeedle)
            rendered = T.pack (show response)
         in rendered /= passwordNeedle
              && not (passwordNeedle `T.isInfixOf` rendered)
      )

propertyHandleRequestWithStatusDoesNotExposeSessionKey 
  :: Agent.AgentState 
  -> Property
propertyHandleRequestWithStatusDoesNotExposeSessionKey initialState =
  let 
    (_, response, _) =
      runMockBitwarden
        defaultMockEnv
        (Agent.handleRequestWith Agent.Status initialState)
  in 
    property $
      not (responseLeaksSessionKey initialState response)

propertyHandleRequestWithListItemsDoesNotExposeSessionKey 
  :: Agent.ItemCacheState
  -> Property
propertyHandleRequestWithListItemsDoesNotExposeSessionKey cacheState =
  let
    sessionKey = Agent.SessionKey "my-session-key"

    currentState =
      Agent.Unlocked sessionKey cacheState

    env =
      defaultMockEnv 
        & withListItemsResult 
          (Left (Agent.ListItemsFailed "list-items should not hit the backend"))

    (_, response, _) =
      runMockBitwarden env
        (Agent.handleRequestWith Agent.ListItems currentState)
 in 
    property $
      not $ sessionKeyAppearsInEncodedResponse sessionKey response

propertyHandleRequestWithGetPasswordDoesNotExposeSessionKey
  :: MockEnv
  -> Agent.ItemCacheState
  -> Agent.SessionKey
  -> Property
propertyHandleRequestWithGetPasswordDoesNotExposeSessionKey mockEnv cacheState sessionKey =
  let 
    loginItemId = Agent.LoginItemId "does-not-matter"

    currentState = Agent.Unlocked sessionKey cacheState

    (_, response, _) =
      runMockBitwarden
        mockEnv
        (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in 
    property $
      not (sessionKeyAppearsInEncodedResponse sessionKey response)


propertyHandleRequestWithUnknownDoesNotExposeSessionKey
  :: Agent.AgentState 
  -> Property
propertyHandleRequestWithUnknownDoesNotExposeSessionKey initialState =
  let
    (_, response, _) =
      runMockBitwarden
        defaultMockEnv
        (Agent.handleRequestWith Agent.UnknownRequest initialState)
  in 
    property $
      not (responseLeaksSessionKey initialState response)

propertyHandleRequestWithDoesNotExposeSecrets
  :: LeakingMockEnv "session-key"
  -> Agent.Request
  -> Agent.ItemCacheState
  -> Property
propertyHandleRequestWithDoesNotExposeSecrets mockEnv request cacheState =
  let
    currentState = Agent.Unlocked (Agent.SessionKey "session-key") cacheState

    (LeakingMockEnv leakingEnv) = mockEnv

    (_, response, _) =
      runMockBitwarden
        leakingEnv
        (Agent.handleRequestWith request currentState)
  in
    property $
      not $ responseLeaksSessionKey currentState response

assertLogsDoNotExposeSecret :: T.Text -> MockEnv -> Agent.Request -> Agent.AgentState -> IO ()
assertLogsDoNotExposeSecret secret mockEnv request initialState =
  assertBool
    ("logs exposed secret " <> show secret <> " in:\n" <> unlines (T.unpack <$> logs))
    (not (any (secret `T.isInfixOf`) logs))
  where
    (_, logs) =
      runLoggingMockBitwarden
        mockEnv
        (loggedHandleRequestWith request initialState)

loggedHandleRequestWith ::
  Agent.Request ->
  Agent.AgentState ->
  LoggingMockBitwarden (Agent.AgentState, Agent.Response, [Agent.Effect])
loggedHandleRequestWith request initialState = do
  recordLog ("received request: " <> T.pack (show request))
  result@(_, response, effects) <- Agent.handleRequestWith request initialState
  recordLog ("sent response: " <> T.pack (show response))
  recordLog ("effects: " <> T.pack (show effects))
  pure result

newtype LoggingMockBitwarden a = LoggingMockBitwarden
  { runLoggingMockBitwardenInternal :: ReaderT MockEnv (WriterT [T.Text] Identity) a
  }
  deriving newtype (Functor, Applicative, Monad)

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
    recordLog ("running bw login: " <> T.pack (show username) <> " " <> T.pack (show password))
    LoggingMockBitwarden (asks unlockResult)
  listItems sessionKey = do
    recordLog ("running bw list items: " <> T.pack (show sessionKey))
    LoggingMockBitwarden (asks listItemsResult)
  sync sessionKey = do
    recordLog ("running bw sync: " <> T.pack (show sessionKey))
    LoggingMockBitwarden (asks syncResult)
  getPassword sessionKey loginItemId = do
    recordLog ("running bw get password: " <> T.pack (show sessionKey) <> " " <> T.pack (show loginItemId))
    LoggingMockBitwarden (asks getPasswordResult)

instance MonadTime LoggingMockBitwarden where
  currentTime =
    LoggingMockBitwarden (asks mockCurrentTime)
  monotonicTime =
    pure 0

adversarialSecret :: T.Text
adversarialSecret = "adversarial-secret"

adversarialEnv :: MockEnv
adversarialEnv =
  let
    LeakingMockEnv mockEnv =
      LeakingMockEnv
        defaultMockEnv
          { unlockResult = Left (Agent.UnlockFailed adversarialSecret),
            listItemsResult = Left (Agent.ListItemsFailed adversarialSecret),
            syncResult = Left (Bitwarden.SyncFailed adversarialSecret),
            getPasswordResult = Left (Bitwarden.GetPasswordFailed adversarialSecret),
            mockCurrentTime = mockNow
          } ::
        LeakingMockEnv "adversarial-secret"
   in
    mockEnv

adversarialUnlockedState :: Agent.AgentState
adversarialUnlockedState =
  Agent.Unlocked
    (Agent.SessionKey adversarialSecret)
    ( Agent.CacheReady
        (Agent.CacheEntry [Agent.ItemSummary "item-123" "Example" "me@example.com"] mockNow)
        Agent.LatestRefreshSucceeded
    )


responseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
responseLeaksSessionKey Agent.Locked _ = False
responseLeaksSessionKey (Agent.Unlocked sessionKey _) response =
  sessionKeyAppearsInEncodedResponse sessionKey response

sessionKeyAppearsInEncodedResponse :: Agent.SessionKey -> Agent.Response -> Bool
sessionKeyAppearsInEncodedResponse (Agent.SessionKey sessionKey) response =
  TE.encodeUtf8 sessionKey `BS.isInfixOf` encodedResponse response

newtype LeakingMockEnv (secret :: Symbol) = LeakingMockEnv MockEnv
  deriving newtype Show

instance KnownSymbol (secret :: Symbol) => Arbitrary (LeakingMockEnv secret) where 
  arbitrary = do
    let 
      secretText = T.pack $ symbolVal (Proxy @secret)
      mockCurrentTime = mockNow

    unlockResult <- elements 
      [ Left Agent.UnlockUnavailable
      , Left (Agent.UnlockFailed secretText)
      , Right (Agent.SessionKey secretText)
      ]
    listItemsResult <- elements
      [ Left Agent.ListItemsUnavailable
      , Left (Agent.ListItemsFailed secretText)
      , Right []
      ]
    syncResult <- elements
      [ Left Bitwarden.SyncUnavailable
      , Left (Bitwarden.SyncFailed secretText)
      ]
    getPasswordResult <- elements
      [ Left Bitwarden.GetPasswordUnavailable 
      , Left (Bitwarden.GetPasswordFailed secretText)
      , Right (Agent.PasswordValue "password")
      ]
    return $ LeakingMockEnv $
      MockEnv {..}
