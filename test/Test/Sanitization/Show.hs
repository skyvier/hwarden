{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sanitization.Show (tests) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import qualified Hwarden.Cache as Cache
import qualified Hwarden.Sanitize as Sanitize
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "show instances"
    [ testGroup
        "secret newtypes"
        [ testProperty "Password show never exposes the password" $
            propertyPasswordShowDoesNotExposePassword
        , testProperty "PasswordValue show never exposes the password" $
            propertyPasswordValueShowDoesNotExposePassword
        , testProperty "SessionKey show never exposes the session key" $
            propertySessionKeyShowDoesNotExposeSessionKey
        ]
    , testGroup
        "instance Show Request"
        [ testProperty "unlock request show never exposes the password" $
            propertyUnlockRequestShowDoesNotExposePassword
        ]
    , testGroup
        "instance Show Response"
        [ testProperty "password-result response show never exposes the plaintext password" $
            propertyPasswordResultShowDoesNotExposePassword
        , testProperty "failure response show never exposes sanitized secrets" $
            propertyFailureResponseShowDoesNotExposeSanitizedSecrets
        ]
    , testGroup
        "agent internals"
        [ testProperty "agent state show never exposes the session key" $
            propertyAgentStateShowDoesNotExposeSessionKey
        , testProperty "effect show never exposes the session key" $
            propertyEffectShowDoesNotExposeSessionKey
        , testProperty "decision show never exposes secrets" $
            propertyDecisionShowDoesNotExposeSecrets
        ]
    , testGroup
        "cache sanitization"
        [ testProperty "CacheFillFailure show never exposes session secrets" $
            propertyCacheFillFailureMappingsDoNotExposeSessionSecrets
        , testProperty "LatestRefreshStatus show never exposes session secrets" $
            propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets
        , testProperty "ItemCacheState show never exposes session secrets" $
            propertyItemCacheStateShowDoesNotExposeSessionSecrets
        ]
    , testGroup
        "BwItem sanitization"
        [ testProperty "BwItem show does not expose login.password secrets" $
            propertyBwItemShowDoesNotExposeLoginPasswordSecret
        , testProperty "BwItem show does not expose hostile unknown secret fields" $
            propertyBwItemShowDoesNotExposeHostileUnknownSecretFields
        ]
    ]

propertyPasswordShowDoesNotExposePassword :: Agent.Password -> Property
propertyPasswordShowDoesNotExposePassword password@(Agent.Password secretText) =
  showDoesNotExpose secretText password

propertyPasswordValueShowDoesNotExposePassword :: Agent.PasswordValue -> Property
propertyPasswordValueShowDoesNotExposePassword passwordValue@(Agent.PasswordValue passwordText) =
  showDoesNotExpose passwordText passwordValue

propertySessionKeyShowDoesNotExposeSessionKey :: Agent.SessionKey -> Property
propertySessionKeyShowDoesNotExposeSessionKey sessionKey@(Agent.SessionKey secretText) =
  showDoesNotExpose secretText sessionKey

propertyUnlockRequestShowDoesNotExposePassword :: Agent.Username -> Agent.Password -> Property
propertyUnlockRequestShowDoesNotExposePassword username password@(Agent.Password secretText) =
  showDoesNotExpose secretText (Agent.UnlockRequest username password)

propertyPasswordResultShowDoesNotExposePassword :: Agent.LoginItemId -> Agent.PasswordValue -> Property
propertyPasswordResultShowDoesNotExposePassword loginItemId passwordValue@(Agent.PasswordValue passwordText) =
  showDoesNotExpose passwordText (Agent.passwordResultResponse loginItemId passwordValue)

propertyFailureResponseShowDoesNotExposeSanitizedSecrets ::
  Agent.Password ->
  Agent.SessionKey ->
  Property
propertyFailureResponseShowDoesNotExposeSanitizedSecrets password@(Agent.Password passwordText) sessionKey@(Agent.SessionKey sessionText) =
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
    showDoesNotExpose passwordText passwordFailure
      .&&. showDoesNotExpose sessionText sessionFailure

propertyAgentStateShowDoesNotExposeSessionKey :: Agent.SessionKey -> Agent.ItemCacheState -> Property
propertyAgentStateShowDoesNotExposeSessionKey sessionKey@(Agent.SessionKey secretText) cacheState =
  showDoesNotExpose secretText (Agent.Unlocked sessionKey cacheState)

propertyEffectShowDoesNotExposeSessionKey :: Agent.SessionKey -> Property
propertyEffectShowDoesNotExposeSessionKey sessionKey@(Agent.SessionKey secretText) =
  showDoesNotExpose secretText (Agent.StartCacheRefreshLoop sessionKey)

propertyDecisionShowDoesNotExposeSecrets ::
  Agent.Username ->
  Agent.Password ->
  Agent.SessionKey ->
  Agent.LoginItemId ->
  Property
propertyDecisionShowDoesNotExposeSecrets username password@(Agent.Password passwordText) sessionKey@(Agent.SessionKey sessionText) loginItemId =
  showDoesNotExpose passwordText (Agent.UnlockAction username password)
    .&&. showDoesNotExpose sessionText (Agent.GetPasswordAction sessionKey loginItemId)

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
   in
    showDoesNotExpose secretText listItemsFailure
      .&&. showDoesNotExpose secretText syncFailure

propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets ::
  Agent.SessionKey ->
  Property
propertyLatestRefreshStatusDoesNotExposeHostileBackendSecrets sessionKey@(Agent.SessionKey secretText) =
  let
    listItemsStatus =
      Agent.LatestRefreshFailed $
        Cache.cacheFillFailureFromListItemsError
          sessionKey
          (Agent.ListItemsFailed ("hostile list-items failure " <> secretText))

    syncStatus =
      Agent.LatestRefreshFailed $
        Cache.syncErrorToCacheFillFailure
          sessionKey
          (Bitwarden.SyncFailed ("hostile sync failure " <> secretText))
   in
    showDoesNotExpose secretText listItemsStatus
      .&&. showDoesNotExpose secretText syncStatus

propertyItemCacheStateShowDoesNotExposeSessionSecrets :: Agent.SessionKey -> Agent.CacheEntry -> Property
propertyItemCacheStateShowDoesNotExposeSessionSecrets sessionKey@(Agent.SessionKey secretText) cacheEntry =
  let
    cacheFillFailure =
      Cache.cacheFillFailureFromListItemsError
        sessionKey
        (Agent.ListItemsFailed ("hostile list-items failure " <> secretText))
   in
    showDoesNotExpose secretText (Agent.CacheFillError cacheFillFailure)
      .&&. showDoesNotExpose secretText (Agent.CacheReady cacheEntry (Agent.LatestRefreshFailed cacheFillFailure))

propertyBwItemShowDoesNotExposeLoginPasswordSecret :: Agent.Password -> Property
propertyBwItemShowDoesNotExposeLoginPasswordSecret (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text)
        , "name" .= ("example item" :: T.Text)
        , "login"
            .= Aeson.object
              [ "username" .= ("me@example.com" :: T.Text)
              , "password" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        showDoesNotExpose passwordNeedle item
      Aeson.Error err ->
        counterexample err False

propertyBwItemShowDoesNotExposeHostileUnknownSecretFields :: Agent.Password -> Property
propertyBwItemShowDoesNotExposeHostileUnknownSecretFields (Agent.Password passwordNeedle) =
  let
    payload =
      Aeson.object
        [ "id" .= ("item-123" :: T.Text)
        , "name" .= ("example item" :: T.Text)
        , "notes" .= passwordNeedle
        , "fields"
            .= [ Aeson.object
                   [ "name" .= ("hostile-field" :: T.Text)
                   , "value" .= passwordNeedle
                   ]
               ]
        , "login"
            .= Aeson.object
              [ "username" .= ("me@example.com" :: T.Text)
              , "totp" .= passwordNeedle
              , "passwordRevisionDate" .= passwordNeedle
              ]
        ]
   in
    case Aeson.fromJSON payload of
      Aeson.Success (item :: Bitwarden.BwItem) ->
        showDoesNotExpose passwordNeedle item
      Aeson.Error err ->
        counterexample err False

showDoesNotExpose :: (Show a) => T.Text -> a -> Property
showDoesNotExpose secretText value =
  let rendered = T.pack (show value)
   in counterexample (show value) $
        not (secretText `T.isInfixOf` rendered)
