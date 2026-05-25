{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Sanitization (tests) where

import qualified Data.Text as T
import Data.Data
import Data.Function ((&))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

import Test.Tasty (TestTree, testGroup)
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

