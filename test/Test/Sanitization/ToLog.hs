{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sanitization.ToLog (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Agent
  ( Effect (..),
    Request (..)
  )
import Hwarden.Logging
  ( ToLog (..),
    field
  )
import Hwarden.Response
  ( CacheAgeSeconds (..),
    FailureMessage (..),
    failureResponse,
    itemListResponse,
    passwordResultResponse,
    successResponse
  )
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (Static),
    sanitizeGetPasswordFailure,
    sanitizeListItemsFailure,
    sanitizeSyncFailure,
    sanitizeUnlockError,
    trustStaticText
  )
import Hwarden.Types
  ( ItemSummary (..),
    LoginItemId (..),
    Password (..),
    PasswordValue (..),
    SessionKey (..),
    Username (..)
  )
import Test.Tasty
  ( TestTree,
    testGroup
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    testCase,
    (@?=)
  )

tests :: TestTree
tests =
  testGroup
    "ToLog sanitization"
    [ testCase "LoginItemId logs the item id" $
        toLogText (LoginItemId "item-123") @?= "item-123"
    , testCase "Username logs the username" $
        toLogText (Username "me@example.com") @?= "me@example.com"
    , testCase "CacheAgeSeconds logs the numeric age" $
        toLogText (CacheAgeSeconds 42) @?= "42"
    , testCase "SanitizedText Static logs trusted static text" $
        toLogText (trustStaticText "safe text" :: SanitizedText Static) @?= "safe text"
    , testCase "SanitizedText PasswordSecret logs sanitized password text" $
        toLogText (sanitizeUnlockError rawPassword ("bad " <> rawPasswordText))
          @?= "bad <redacted>"
    , testCase "SanitizedText SessionSecret logs sanitized session text" $
        toLogText (sanitizeListItemsFailure rawSessionKey ("bad " <> rawSessionKeyText))
          @?= "bad <redacted>"
    , testCase "FailureMessage logs static failures" $
        toLogText (StaticFailure (trustStaticText "safe failure"))
          @?= "safe failure"
    , testCase "FailureMessage logs password-sanitized failures" $
        assertLogDoesNotExpose
          [rawPasswordText]
          (PasswordSanitizedFailure (sanitizeUnlockError rawPassword ("bad " <> rawPasswordText)))
    , testCase "FailureMessage logs session-sanitized failures" $
        assertLogDoesNotExpose
          [rawSessionKeyText]
          (SessionSanitizedFailure (sanitizeGetPasswordFailure rawSessionKey ("bad " <> rawSessionKeyText)))
    , testCase "Response logs success messages" $
        toLogText (successResponse "unlocked") @?= "Success unlocked"
    , testCase "Response logs item lists without item fields" $
        let response =
              itemListResponse
                [ItemSummary "item-secret" "name-secret" "username-secret"]
                (CacheAgeSeconds 7)
         in do
              toLogText response @?= "[ItemList] aged 7"
              assertTextDoesNotExpose
                ["item-secret", "name-secret", "username-secret"]
                (toLogText response)
    , testCase "Response logs password results without the password" $
        assertLogDoesNotExpose
          [rawPasswordValueText]
          (passwordResultResponse (LoginItemId "item-123") rawPasswordValue)
    , testCase "Response logs failures through FailureMessage" $
        assertLogDoesNotExpose
          [rawSessionKeyText]
          (failureResponse (SessionSanitizedFailure (sanitizeSyncFailure rawSessionKey ("bad " <> rawSessionKeyText))))
    , testCase "Request logs unlock requests without the password" $
        assertLogDoesNotExpose
          [rawPasswordText]
          (UnlockRequest (Username "me@example.com") rawPassword)
    , testCase "Request logs status" $
        toLogText Status @?= "status"
    , testCase "Request logs list-items" $
        toLogText ListItems @?= "list-items"
    , testCase "Request logs get-password item ids" $
        toLogText (GetPasswordRequest (LoginItemId "item-123"))
          @?= "get-password (item-123)"
    , testCase "Request logs unknown requests" $
        toLogText UnknownRequest @?= "unknown-request"
    , testCase "Effect logs cache refresh loop starts without the session key" $
        assertLogDoesNotExpose
          [rawSessionKeyText]
          (StartCacheRefreshLoop rawSessionKey)
    , testCase "Field logs with the wrapped value renderer" $
        toLogText (field @"chosen_identifier" (LoginItemId "item-123"))
          @?= "item-123"
    , testCase "Password logs as redacted text" $
        assertLogRedacts rawPasswordText rawPassword
    , testCase "PasswordValue logs as redacted text" $
        assertLogRedacts rawPasswordValueText rawPasswordValue
    , testCase "SessionKey logs as redacted text" $
        assertLogRedacts rawSessionKeyText rawSessionKey
    ]

rawPassword :: Password
rawPassword = Password rawPasswordText

rawPasswordText :: Text
rawPasswordText = "raw-password-secret"

rawPasswordValue :: PasswordValue
rawPasswordValue = PasswordValue rawPasswordValueText

rawPasswordValueText :: Text
rawPasswordValueText = "raw-password-value-secret"

rawSessionKey :: SessionKey
rawSessionKey = SessionKey rawSessionKeyText

rawSessionKeyText :: Text
rawSessionKeyText = "raw-session-key-secret"

assertLogRedacts :: ToLog value => Text -> value -> Assertion
assertLogRedacts secret value = do
  let rendered = toLogText value
  rendered @?= "[REDACTED]"
  assertTextDoesNotExpose [secret] rendered

assertLogDoesNotExpose :: ToLog value => [Text] -> value -> Assertion
assertLogDoesNotExpose secrets =
  assertTextDoesNotExpose secrets . toLogText

assertTextDoesNotExpose :: [Text] -> Text -> Assertion
assertTextDoesNotExpose secrets rendered =
  assertBool
    ("expected log text not to expose secrets, got: " <> T.unpack rendered)
    (all (not . (`T.isInfixOf` rendered)) secrets)
