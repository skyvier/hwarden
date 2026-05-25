{-# LANGUAGE OverloadedStrings #-}

module Test.JsonCodec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson as Aeson
import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS

tests :: TestTree
tests = testGroup "JSON codec"
  [ decodeTests
  , encodeTests 
  , roundTripTests
  ]

roundTripTests :: TestTree
roundTripTests = testGroup "decode . encode <=> id" 
  [ -- TODO
  ]

decodeTests :: TestTree
decodeTests =
  testGroup
    "JSON decoding"
    [ testCase "request parser decodes unlock payload" $ do
        let payload =
                "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right
            ( Agent.UnlockRequest
                (Agent.Username "me@example.com")
                (Agent.Password "bad-password")
            )
    , testCase "bitwarden item parser decodes a login item" $ do
        let payload =
                "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":\"skyvier\"}}]"
        Aeson.eitherDecodeStrict payload
          @?= Right [
            Bitwarden.BwItem 
              "1" 
              "Battle.net" 
              (Just $ Bitwarden.BwLogin (Just "skyvier"))
            ]
    , testCase "bitwarden item parser tolerates null login username" $ do
        let payload =
                "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":null}}]"
        Aeson.eitherDecodeStrict payload
          @?= Right [
            Bitwarden.BwItem
              "1"
              "Battle.net"
              (Just $ Bitwarden.BwLogin Nothing)
            ]
    , testCase "bitwarden item parser decodes non-login items too" $ do
        let payload =
                "[{\"id\":\"1\",\"name\":\"Secure note\"}]"
        Aeson.eitherDecodeStrict payload
          @?= Right [
            Bitwarden.BwItem 
              "1" 
              "Secure note" 
              Nothing
            ]
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
    ]

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
    , testCase "item-list response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/item-list.json"
          ( Agent.itemListResponse
              [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com",
                Agent.ItemSummary "2" "GitHub" "skyvier"
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

