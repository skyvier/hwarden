{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sanitization.Json (tests) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.TypeLits
import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import qualified Hwarden.Sanitize as Sanitize
import Test.Helpers
import Test.MockEnv
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "JSON sanitization"
    [ testGroup
        "instance ToJSON Response"
        [ testProperty "failure response encoding never exposes sanitized secrets" $
            propertyFailureResponseJsonDoesNotExposeSanitizedSecrets
        ]
    , testGroup
        "instance ToJSON ItemSummary"
        [ testProperty "encoded login item summaries do not expose login.password secrets" $
            propertyEncodedLoginItemSummariesDoNotExposeLoginPasswordSecret
        , testProperty "encoded login item summaries do not expose hostile unknown secret fields" $
            propertyEncodedLoginItemSummariesDoNotExposeHostileUnknownSecretFields
        ]
    , testGroup
        "handler response encoding"
        [ testProperty "given an unlocked state, bitwarden CLI outputs are always redacted of secrets" $
            propertyHandleRequestWithDoesNotExposeSecrets
        ]
    ]

propertyFailureResponseJsonDoesNotExposeSanitizedSecrets ::
  Agent.Password ->
  Agent.SessionKey ->
  Property
propertyFailureResponseJsonDoesNotExposeSanitizedSecrets password@(Agent.Password passwordText) sessionKey@(Agent.SessionKey sessionText) =
  let
    passwordFailure =
      Agent.failureResponse $
        Agent.PasswordSanitizedFailure $
          Agent.sanitizeUnlockError password ("hostile unlock failure " <> passwordText)

    sessionFailure =
      Agent.failureResponse $
        Agent.SessionSanitizedFailure $
          Sanitize.sanitizeGetPasswordFailure sessionKey ("hostile get-password failure " <> sessionText)
  in
    encodedResponseDoesNotExpose passwordText passwordFailure
      .&&. encodedResponseDoesNotExpose sessionText sessionFailure

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

propertyHandleRequestWithDoesNotExposeSecrets ::
  LeakingMockEnv "session-key" ->
  Agent.Request ->
  Agent.ItemCacheState ->
  Property
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

responseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
responseLeaksSessionKey Agent.Locked _ = False
responseLeaksSessionKey (Agent.Unlocked sessionKey _) response =
  sessionKeyAppearsInEncodedResponse sessionKey response

sessionKeyAppearsInEncodedResponse :: Agent.SessionKey -> Agent.Response -> Bool
sessionKeyAppearsInEncodedResponse (Agent.SessionKey sessionKey) response =
  TE.encodeUtf8 sessionKey `BS.isInfixOf` encodedResponse response

encodedResponseDoesNotExpose :: T.Text -> Agent.Response -> Property
encodedResponseDoesNotExpose secretText response =
  let rendered = encodedResponse response
   in counterexample (BS.unpack rendered) $
        not (TE.encodeUtf8 secretText `BS.isInfixOf` rendered)

newtype LeakingMockEnv (secret :: Symbol) = LeakingMockEnv MockEnv
  deriving newtype (Show)

instance KnownSymbol (secret :: Symbol) => Arbitrary (LeakingMockEnv secret) where
  arbitrary = do
    let
      secretText = T.pack $ symbolVal (Proxy @secret)
      mockCurrentTime = mockNow

    unlockResult <-
      elements
        [ Left Agent.UnlockUnavailable,
          Left (Agent.UnlockFailed secretText),
          Right (Agent.SessionKey secretText)
        ]
    listItemsResult <-
      elements
        [ Left Agent.ListItemsUnavailable,
          Left (Agent.ListItemsFailed secretText),
          Right []
        ]
    syncResult <-
      elements
        [ Left Bitwarden.SyncUnavailable,
          Left (Bitwarden.SyncFailed secretText)
        ]
    getPasswordResult <-
      elements
        [ Left Bitwarden.GetPasswordUnavailable,
          Left (Bitwarden.GetPasswordFailed secretText),
          Right (Agent.PasswordValue "password")
        ]
    return $ LeakingMockEnv $ MockEnv {..}
