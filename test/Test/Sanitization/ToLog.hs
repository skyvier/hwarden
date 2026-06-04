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
    [ testGroup "secret newtypes" 
      [ testCase "Password logs as redacted text" $
          toLogText (Password "secret") @?= "[REDACTED]"
      , testCase "PasswordValue logs as redacted text" $
          toLogText (PasswordValue "secret") @?= "[REDACTED]"
      , testCase "SessionKey logs as redacted text" $
          toLogText (SessionKey "secret") @?= "[REDACTED]"
      ]
    , testGroup "non-secret newtypes"
      [ testCase "Username logs the username" $
          toLogText (Username "me@example.com") @?= "me@example.com"
      , testCase "CacheAgeSeconds logs the numeric age" $
          toLogText (CacheAgeSeconds 42) @?= "42"
      , testCase "LoginItemId logs the item id" $
          toLogText (LoginItemId "item-123") @?= "item-123"
      ]
    , testGroup "sanitized text"
      [ testCase "SanitizedText Static logs trusted static text" $
          toLogText (trustStaticText "safe text" :: SanitizedText Static) @?= "safe text"
      , testCase "SanitizedText PasswordSecret logs sanitized password text" $
          toLogText (sanitizeUnlockError rawPassword ("bad " <> rawPasswordText))
            @?= "bad <redacted>"
      , testCase "SanitizedText SessionSecret logs sanitized session text" $
          toLogText (sanitizeListItemsFailure rawSessionKey ("bad " <> rawSessionKeyText))
            @?= "bad <redacted>"
      ]
    , testGroup "failure message" 
      [ testCase "FailureMessage logs static failures" $
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
      ]
    , testGroup "Response"
      [ testCase "Success" $
          toLogText (successResponse "unlocked") @?= "Success unlocked"
      , testCase "ItemList logs item list without item fields" $
          let response =
                itemListResponse
                  [ItemSummary "item-secret" "name-secret" "username-secret"]
                  (CacheAgeSeconds 7)
           in do
                toLogText response @?= "[ItemList] aged 7"
                assertTextDoesNotExpose
                  ["item-secret", "name-secret", "username-secret"]
                  (toLogText response)
      , testCase "PasswordResult does not expose raw password" $
          assertLogDoesNotExpose
            [rawPasswordValueText]
            (passwordResultResponse (LoginItemId "item-123") rawPasswordValue)
      , testCase "Failure logs failures through FailureMessage" $
          assertLogDoesNotExpose
            [rawSessionKeyText]
            (failureResponse (SessionSanitizedFailure (sanitizeSyncFailure rawSessionKey ("bad " <> rawSessionKeyText))))
      ]
    , testGroup "Request"
      [ testCase "UnlockRequest does not expose password" $
          assertLogDoesNotExpose
            [rawPasswordText]
            (UnlockRequest (Username "me@example.com") rawPassword)
      , testCase "Status request" $
          toLogText Status @?= "status"
      , testCase "ListItems request" $
          toLogText ListItems @?= "list-items"
      , testCase "GetPasswordRequest" $
          toLogText (GetPasswordRequest (LoginItemId "item-123"))
            @?= "get-password (item-123)"
      , testCase "UnknownRequest" $
          toLogText UnknownRequest @?= "unknown-request"
      ]
    , testCase "Effect logs cache refresh loop starts without the session key" $
        assertLogDoesNotExpose
          [rawSessionKeyText]
          (StartCacheRefreshLoop rawSessionKey)
    , testCase "Field logs with the wrapped value renderer" $
        toLogText (field @"chosen_identifier" (LoginItemId "item-123"))
          @?= "item-123"
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

assertLogDoesNotExpose :: ToLog value => [Text] -> value -> Assertion
assertLogDoesNotExpose secrets =
  assertTextDoesNotExpose secrets . toLogText

assertTextDoesNotExpose :: [Text] -> Text -> Assertion
assertTextDoesNotExpose secrets rendered =
  assertBool
    ("expected log text not to expose secrets, got: " <> T.unpack rendered) $
    not (any (`T.isInfixOf` rendered) secrets)
