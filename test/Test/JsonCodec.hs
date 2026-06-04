{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.JsonCodec (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Sanitize (SanitizedText, Secret (PasswordSecret, SessionSecret), trustStaticText)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden

tests :: TestTree
tests =
  testGroup
    "JSON codec"
    [ decodeTests
    , encodeTests
    , roundTripTests
    ]

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "decode . encode <=> id"
    [ testProperty "Command" $ \cmd ->
        Agent.fromCommandIdentifier (Agent.toCommandIdentifier cmd)
          === Just (cmd :: Agent.Command)
    , testProperty "Request" $ \request ->
        Aeson.eitherDecode (Aeson.encode request) === Right (request :: Agent.Request)
    , testProperty "ItemSummary" $ \itemSummary ->
        Aeson.eitherDecode (Aeson.encode itemSummary) === Right (itemSummary :: Agent.ItemSummary)
    , -- Response failures decode back through StaticFailure regardless of whether
      -- they were originally static, password-sanitized, or session-sanitized.
      -- Compare the JSON form so the roundtrip test checks the external contract.
      testProperty "Response preserves JSON through decode/encode" $
        forAll arbitraryResponse $ \response ->
          fmap Aeson.toJSON (Aeson.eitherDecode (Aeson.encode response) :: Either String Agent.Response)
            === Right (Aeson.toJSON response)
    ]

arbitraryResponse :: Gen Agent.Response
arbitraryResponse =
  oneof
    [ Agent.successResponse . T.pack <$> arbitrary
    , Agent.itemListResponse <$> arbitrary <*> arbitrary
    , Agent.passwordResultResponse <$> arbitrary <*> arbitrary
    , Agent.failureResponse <$> arbitraryFailureMessage
    ]

arbitraryFailureMessage :: Gen Agent.FailureMessage
arbitraryFailureMessage =
  oneof
    [ Agent.StaticFailure . trustStaticText . T.pack <$> arbitrary
    , Agent.PasswordSanitizedFailure <$> (arbitrary :: Gen (SanitizedText PasswordSecret))
    , Agent.SessionSanitizedFailure <$> (arbitrary :: Gen (SanitizedText SessionSecret))
    ]

newtype JsonObject = JsonObject Aeson.Value
  deriving (Eq, Show)

arbitraryJsonObjectWithCmd :: Text -> Gen JsonObject
arbitraryJsonObjectWithCmd cmdIdentifier = do
  value <- arbitrary

  let obj =
        case value of
          Aeson.Object o -> o
          _ -> KM.empty

  pure $
    JsonObject $
      Aeson.Object $
        KM.insert
          (Key.fromString "cmd")
          (Aeson.String cmdIdentifier)
          obj

decodeTests :: TestTree
decodeTests =
  testGroup
    "JSON decoding"
    [ testGroup
        "Request"
        [ testCase "request parser decodes unlock payload" $ do
            let payload =
                  "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right
                ( Agent.UnlockRequest
                    (Agent.Username "me@example.com")
                    (Agent.Password "bad-password")
                )
        , testCase "request parser decodes status payload" $ do
            let payload = "{\"cmd\":\"status\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right Agent.Status
        , testCase "request parser decodes list-items payload" $ do
            let payload = "{\"cmd\":\"list-items\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right Agent.ListItems
        , testCase "request parser decodes get-password payload" $ do
            let payload = "{\"cmd\":\"get-password\",\"id\":\"item-123\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
        , testCase "request parser rejects unlock payload without email" $ do
            let payload = "{\"cmd\":\"unlock\",\"password\":\"bad-password\"}"
            assertBool
              "expected unlock payload without email to fail decoding"
              (isLeftDecodeFailure (Aeson.eitherDecodeStrict' payload :: Either String Agent.Request))
        , testCase "request parser rejects unlock payload without password" $ do
            let payload = "{\"cmd\":\"unlock\",\"email\":\"me@example.com\"}"
            assertBool
              "expected unlock payload without password to fail decoding"
              (isLeftDecodeFailure (Aeson.eitherDecodeStrict' payload :: Either String Agent.Request))
        , testCase "request parser rejects get-password payload without id" $ do
            let payload = "{\"cmd\":\"get-password\"}"
            assertBool
              "expected get-password payload without id to fail decoding"
              (isLeftDecodeFailure (Aeson.eitherDecodeStrict' payload :: Either String Agent.Request))
        , testCase "request parser rejects get-password payload with non-text id" $ do
            let payload = "{\"cmd\":\"get-password\",\"id\":123}"
            assertBool
              "expected get-password payload with non-text id to fail decoding"
              (isLeftDecodeFailure (Aeson.eitherDecodeStrict' payload :: Either String Agent.Request))
        , testProperty "request parser decodes unknown command payload" $ \cmdIdStr ->
            let
              cmdId = T.pack cmdIdStr
             in
              isUnknownCommand cmdId ==>
                forAll (arbitraryJsonObjectWithCmd cmdId) $ \(JsonObject value) ->
                  let
                    payload = Aeson.encode value
                   in
                    Aeson.eitherDecode payload
                      === Right Agent.UnknownRequest
        ]
    , testGroup
        "Response"
        [ testCase "response parser decodes success payload" $ do
            let payload = "{\"ok\":true,\"message\":\"unlocked\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right (Agent.successResponse "unlocked")
        , testCase "response parser decodes item-list payload" $ do
            let payload =
                  "{\"ok\":true,\"items\":[{\"id\":\"1\",\"name\":\"Battle.net\",\"username\":\"skyvier\"}],\"cache_age_seconds\":5}"
            Aeson.eitherDecodeStrict' payload
              @?= Right
                ( Agent.itemListResponse
                    [Agent.ItemSummary "1" "Battle.net" "skyvier"]
                    (Agent.CacheAgeSeconds 5)
                )
        , testCase "response parser decodes password-result payload" $ do
            let payload = "{\"ok\":true,\"id\":\"item-123\",\"password\":\"super-secret\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right
                (Agent.passwordResultResponse (Agent.LoginItemId "item-123") (Agent.PasswordValue "super-secret"))
        , testCase "response parser decodes failure payload" $ do
            let payload = "{\"ok\":false,\"error\":\"boom\"}"
            Aeson.eitherDecodeStrict' payload
              @?= Right (Agent.failureResponse "boom")
        ]
    , testGroup
        "BwItem"
        [ testCase "bitwarden item parser decodes a login item" $ do
            let payload =
                  "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":\"skyvier\"}}]"
            Aeson.eitherDecodeStrict payload
              @?= Right
                [ Bitwarden.BwItem
                    "1"
                    "Battle.net"
                    (Just $ Bitwarden.BwLogin (Just "skyvier"))
                ]
        , testCase "bitwarden item parser tolerates null login username" $ do
            let payload =
                  "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":null}}]"
            Aeson.eitherDecodeStrict payload
              @?= Right
                [ Bitwarden.BwItem
                    "1"
                    "Battle.net"
                    (Just $ Bitwarden.BwLogin Nothing)
                ]
        , testCase "bitwarden item parser decodes non-login items too" $ do
            let payload =
                  "[{\"id\":\"1\",\"name\":\"Secure note\"}]"
            Aeson.eitherDecodeStrict payload
              @?= Right
                [ Bitwarden.BwItem
                    "1"
                    "Secure note"
                    Nothing
                ]
        , testCase "bitwarden item parser ignores extra attributes" $ do
            let payload =
                  "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":\"skyvier\"},\"extra\": \"ignored\"}]"
            Aeson.eitherDecodeStrict payload
              @?= Right
                [ Bitwarden.BwItem
                    "1"
                    "Battle.net"
                    (Just $ Bitwarden.BwLogin (Just "skyvier"))
                ]
        , testCase "bitwarden item parser tolerates login object without username" $ do
            let payload =
                  "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{}}]"
            Aeson.eitherDecodeStrict payload
              @?= Right
                [ Bitwarden.BwItem
                    "1"
                    "Battle.net"
                    (Just $ Bitwarden.BwLogin Nothing)
                ]
        , testCase "bitwarden item parser rejects items without id" $ do
            let payload =
                  "[{\"name\":\"Battle.net\",\"login\":{\"username\":\"skyvier\"}}]"
            assertBool
              "expected item without id to fail decoding"
              (isLeftDecodeFailure (Aeson.eitherDecodeStrict payload :: Either String [Bitwarden.BwItem]))
        ]
    ]

isUnknownCommand :: Text -> Bool
isUnknownCommand commandIdentifier =
  isNothing (Agent.fromCommandIdentifier commandIdentifier)

isLeftDecodeFailure :: Either String a -> Bool
isLeftDecodeFailure = either (const True) (const False)

encodeTests :: TestTree
encodeTests =
  testGroup
    "encoding"
    [ testCase "success response encoding matches golden file" $
        assertGoldenEncoding "test/golden/success.json" (Agent.successResponse "unlocked")
    , testCase "unlock request encoding matches expected shape" $
        Aeson.encode
          ( Agent.UnlockRequest
              (Agent.Username "me@example.com")
              (Agent.Password "bad-password")
          )
          @?= "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"
    , testCase "failure response encoding matches golden file" $
        assertGoldenEncoding "test/golden/failure.json" (Agent.failureResponse "boom")
    , testCase "failure response encoding escapes quotes and newlines" $
        assertGoldenEncoding
          "test/golden/failure-escaped.json"
          (Agent.failureResponse "boom \"quoted\"\nnext line")
    , testCase "item-list response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/item-list.json"
          ( Agent.itemListResponse
              [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com"
              , Agent.ItemSummary "2" "GitHub" "skyvier"
              ]
              (Agent.CacheAgeSeconds 0)
          )
    , testCase "password response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/password-result.json"
          (Agent.passwordResultResponse (Agent.LoginItemId "item-123") (Agent.PasswordValue "super-secret"))
    ]

assertGoldenEncoding :: FilePath -> Agent.Response -> IO ()
assertGoldenEncoding goldenPath response = do
  expected <- BS.readFile goldenPath
  let normalizedExpected = BS8.dropWhileEnd (== '\n') expected
  LBS.toStrict (Aeson.encode response) @?= normalizedExpected
