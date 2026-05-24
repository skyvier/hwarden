{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Response
  ( CacheAgeSeconds (..),
    FailureMessage (..),
    Response,
    failureResponse,
    itemListResponse,
    passwordResultResponse,
    responseErrorText,
    responseItems,
    responsePasswordResult,
    responseIsFailure,
    successResponse,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
    (.=)
  )
import Data.String (IsString (fromString))
import Data.Text (Text)
import Hwarden.Sanitize
  ( SanitizedText,
    Secret (PasswordSecret, SessionSecret, Static),
    getSanitizedText,
    trustStaticText
  )
import Hwarden.Types
  ( ItemSummary,
    LoginItemId (LoginItemId),
    PasswordValue (PasswordValue)
  )
import Test.QuickCheck (Arbitrary (arbitrary), NonNegative (getNonNegative))

newtype CacheAgeSeconds = CacheAgeSeconds Int
  deriving (Eq, Show)

instance Arbitrary CacheAgeSeconds where
  arbitrary = CacheAgeSeconds . getNonNegative <$> arbitrary

data FailureMessage
  = StaticFailure (SanitizedText Static)
  | PasswordSanitizedFailure (SanitizedText PasswordSecret)
  | SessionSanitizedFailure (SanitizedText SessionSecret)
  deriving (Eq, Show)

instance IsString FailureMessage where
  fromString = StaticFailure . fromString

data Response
  = Success Text
  | ItemList [ItemSummary] CacheAgeSeconds
  | PasswordResult LoginItemId PasswordValue
  | Failure FailureMessage
  deriving (Eq)

instance Show Response where
  show (Success message) = "Success " <> show message
  show (ItemList items cacheAgeSecondsValue) = "ItemList " <> show items <> " " <> show cacheAgeSecondsValue
  show (PasswordResult loginItemId password) = "PasswordResult " <> show loginItemId <> " " <> show password
  show (Failure err) = "Failure " <> show err

successResponse :: Text -> Response
successResponse = Success

itemListResponse :: [ItemSummary] -> CacheAgeSeconds -> Response
itemListResponse = ItemList

passwordResultResponse :: LoginItemId -> PasswordValue -> Response
passwordResultResponse = PasswordResult

failureResponse :: FailureMessage -> Response
failureResponse = Failure

responseErrorText :: Response -> Maybe Text
responseErrorText (Failure failureMessage) = Just (renderFailureMessage failureMessage)
responseErrorText _ = Nothing

responseItems :: Response -> Maybe ([ItemSummary], CacheAgeSeconds)
responseItems (ItemList items cacheAgeSecondsValue) = Just (items, cacheAgeSecondsValue)
responseItems _ = Nothing

responsePasswordResult :: Response -> Maybe (LoginItemId, PasswordValue)
responsePasswordResult (PasswordResult loginItemId password) = Just (loginItemId, password)
responsePasswordResult _ = Nothing

responseIsFailure :: Response -> Bool
responseIsFailure Failure {} = True
responseIsFailure _ = False

renderFailureMessage :: FailureMessage -> Text
renderFailureMessage failureMessage =
  case failureMessage of
    StaticFailure message ->
      getSanitizedText message
    PasswordSanitizedFailure message ->
      getSanitizedText message
    SessionSanitizedFailure message ->
      getSanitizedText message

instance ToJSON Response where
  toJSON (Success message) =
    object
      [ "ok" .= True,
        "message" .= message
      ]
  toJSON (ItemList items (CacheAgeSeconds cacheAgeSecondsValue)) =
    object
      [ "ok" .= True,
        "items" .= items,
        "cache_age_seconds" .= cacheAgeSecondsValue
      ]
  toJSON (PasswordResult (LoginItemId passwordItemId) (PasswordValue password)) =
    object
      [ "ok" .= True,
        "id" .= passwordItemId,
        "password" .= password
      ]
  toJSON (Failure err) =
    object
      [ "ok" .= False,
        "error" .= renderFailureMessage err
      ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then
        (PasswordResult . LoginItemId <$> obj .: "id" <*> (PasswordValue <$> obj .: "password"))
          <|> (ItemList <$> obj .: "items" <*> (CacheAgeSeconds <$> obj .: "cache_age_seconds"))
          <|> (Success <$> obj .: "message")
      else Failure . StaticFailure . trustStaticText <$> obj .: "error"
