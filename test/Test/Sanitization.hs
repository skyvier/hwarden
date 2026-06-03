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
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as T
import Data.Data
import Data.Function ((&))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.MockEnv
import Test.Helpers

import GHC.TypeLits

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import Hwarden.Logging
import Control.Monad (forM_)
import qualified Hwarden.Cache as Cache

tests :: TestTree
tests = testGroup "sanitization"
  [ secretRedactionTests
  , showTests
  ]

secretRedactionTests :: TestTree
secretRedactionTests = testGroup "secret redaction invariants"
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
  , testProperty "encoded login item summaries do not expose login.password secrets" $
      propertyEncodedLoginItemSummariesDoNotExposeLoginPasswordSecret
  , testProperty "encoded login item summaries do not expose hostile unknown secret fields" $
      propertyEncodedLoginItemSummariesDoNotExposeHostileUnknownSecretFields
  , testProperty "CacheFillFailure mappings never expose session secrets" $
      propertyCacheFillFailureMappingsDoNotExposeSessionSecrets
  , testProperty "LatestRefreshStatus built from a hostile backend never exposes secrets" $
      propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets
  ]

showTests :: TestTree
showTests = testGroup "show instances"
  [ testProperty "given a successful get-password response, show never exposes the plaintext password" $
      propertyPasswordResultShowDoesNotExposePassword
  , testProperty "BwItem show does not expose login.password secrets" $
      propertyBwItemShowDoesNotExposeLoginPasswordSecret
  , testProperty "BwItem show does not expose hostile unknown secret fields" $
      propertyBwItemShowDoesNotExposeHostileUnknownSecretFields
  ]

propertyBwItemShowDoesNotExposeLoginPasswordSecret :: Agent.Password -> Property
propertyBwItemShowDoesNotExposeLoginPasswordSecret (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text),
          "name" .= ("example item" :: T.Text),
          "login" .=
            Aeson.object
              [ "username" .= ("me@example.com" :: T.Text),
                "password" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        let
          renderedItem = T.pack (show item)
        in
          counterexample (show item) $
            not (passwordNeedle `T.isInfixOf` renderedItem)
      Aeson.Error err ->
        counterexample err False

propertyEncodedLoginItemSummariesDoNotExposeLoginPasswordSecret :: Agent.Password -> Property
propertyEncodedLoginItemSummariesDoNotExposeLoginPasswordSecret (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text),
          "name" .= ("example item" :: T.Text),
          "login" .=
            Aeson.object
              [ "username" .= ("me@example.com" :: T.Text),
                "password" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        let
          encodedSummaries = LBS.toStrict (Aeson.encode (Bitwarden.extractLoginItems [item]))
          passwordBytes = TE.encodeUtf8 passwordNeedle
        in
          counterexample (BS.unpack encodedSummaries) $
            not (passwordBytes `BS.isInfixOf` encodedSummaries)
      Aeson.Error err ->
        counterexample err False

propertyBwItemShowDoesNotExposeHostileUnknownSecretFields :: Agent.Password -> Property
propertyBwItemShowDoesNotExposeHostileUnknownSecretFields (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text),
          "name" .= ("example item" :: T.Text),
          "notes" .= passwordNeedle,
          "fields" .=
            [ Aeson.object
                [ "name" .= ("hostile-field" :: T.Text),
                  "value" .= passwordNeedle
                ]
            ],
          "login" .=
            Aeson.object
              [ "username" .= ("me@example.com" :: T.Text),
                "totp" .= passwordNeedle,
                "passwordRevisionDate" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        let
          renderedItem = T.pack (show item)
        in
          counterexample (show item) $
            not (passwordNeedle `T.isInfixOf` renderedItem)
      Aeson.Error err ->
        counterexample err False

propertyEncodedLoginItemSummariesDoNotExposeHostileUnknownSecretFields :: Agent.Password -> Property
propertyEncodedLoginItemSummariesDoNotExposeHostileUnknownSecretFields (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text),
          "name" .= ("example item" :: T.Text),
          "notes" .= passwordNeedle,
          "fields" .=
            [ Aeson.object
                [ "name" .= ("hostile-field" :: T.Text),
                  "value" .= passwordNeedle
                ]
            ],
          "login" .=
            Aeson.object
              [ "username" .= ("me@example.com" :: T.Text),
                "totp" .= passwordNeedle,
                "passwordRevisionDate" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        let
          encodedSummaries = LBS.toStrict (Aeson.encode (Bitwarden.extractLoginItems [item]))
          passwordBytes = TE.encodeUtf8 passwordNeedle
        in
          counterexample (BS.unpack encodedSummaries) $
            not (passwordBytes `BS.isInfixOf` encodedSummaries)
      Aeson.Error err ->
        counterexample err False

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
  :: LeakingMockEnv "session-key" "password-value"
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

propertyCacheFillFailureMappingsDoNotExposeSessionSecrets :: Agent.SessionKey -> Property
propertyCacheFillFailureMappingsDoNotExposeSessionSecrets sessionKey@(Agent.SessionKey secretText) =
  let
    listItemsFailure =
      Cache.cacheFillFailureFromListItemsError
        sessionKey
        (Agent.ListItemsFailed ("hostile list-items failure " <> secretText))

    syncFailure =
      Cache.syncErrorToCacheFillFailure
        sessionKey
        (Bitwarden.SyncFailed ("hostile sync failure " <> secretText))

    doesNotLeak cacheFillFailure =
      let rendered = T.pack (show cacheFillFailure)
       in counterexample (show cacheFillFailure) $
            not (secretText `T.isInfixOf` rendered)
  in
    doesNotLeak listItemsFailure .&&. doesNotLeak syncFailure

propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets
  :: LeakingMockEnv "latest-refresh-status-secret" "latest-refresh-status-secret"
  -> Agent.CacheEntry
  -> Property
propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets mockEnv cacheEntry =
  let
    secretText = "latest-refresh-status-secret"
    sessionKey = Agent.SessionKey secretText

    (LeakingMockEnv leakingEnv) = mockEnv
    hostileSyncEnv =
      leakingEnv
        & withSyncResult (Left (Bitwarden.SyncFailed secretText))

    hostileListItemsEnv =
      leakingEnv
        & withSyncResult (Right ())
        & withListItemsResult (Left (Agent.ListItemsFailed secretText))

    latestRefreshStatusFrom env =
      case Cache.updateItemCacheState
        (Agent.CacheReady cacheEntry Agent.LatestRefreshSucceeded)
        (runMockBitwarden env (Cache.refreshCacheEntry sessionKey)) of
        Agent.CacheReady _ status -> status
        cacheState ->
          error
            ( "expected failed refresh to preserve ready cache, got "
                <> show cacheState
            )

    doesNotLeak latestRefreshStatus =
      let rendered = T.pack (show latestRefreshStatus)
       in counterexample (show latestRefreshStatus) $
            not (secretText `T.isInfixOf` rendered)
  in
    doesNotLeak (latestRefreshStatusFrom hostileSyncEnv)
      .&&. doesNotLeak (latestRefreshStatusFrom hostileListItemsEnv)

responseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
responseLeaksSessionKey Agent.Locked _ = False
responseLeaksSessionKey (Agent.Unlocked sessionKey _) response =
  sessionKeyAppearsInEncodedResponse sessionKey response

sessionKeyAppearsInEncodedResponse :: Agent.SessionKey -> Agent.Response -> Bool
sessionKeyAppearsInEncodedResponse (Agent.SessionKey sessionKey) response =
  TE.encodeUtf8 sessionKey `BS.isInfixOf` encodedResponse response

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
