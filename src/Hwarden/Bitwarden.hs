{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Bitwarden
  ( Bitwarden (..),
    decodeItemSummaries,
    ListItemsError (..),
    UnlockError (..)
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Types
  ( ItemSummary (ItemSummary),
    Password (Password),
    SessionKey (SessionKey),
    Username (Username)
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Environment (getEnvironment)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data UnlockError
  = UnlockUnavailable
  | UnlockFailed Text
  deriving (Eq, Show)

data ListItemsError
  = ListItemsUnavailable
  | ListItemsFailed Text
  deriving (Eq, Show)

instance Arbitrary UnlockError where
  arbitrary =
    oneof
      [ pure UnlockUnavailable,
        UnlockFailed . T.pack <$> arbitrary
      ]

instance Arbitrary ListItemsError where
  arbitrary =
    oneof
      [ pure ListItemsUnavailable,
        ListItemsFailed . T.pack <$> arbitrary
      ]

class Monad m => Bitwarden m where
  unlock :: Username -> Password -> m (Either UnlockError SessionKey)
  listItems :: SessionKey -> m (Either ListItemsError [ItemSummary])

instance Bitwarden IO where
  unlock (Username email) (Password password) = do
    let args = [T.unpack email, T.unpack password, "--raw"]
    result <-
      try (readProcessWithExitCode "bw" ("login" : args) "") ::
        IO (Either SomeException (ExitCode, String, String))
    pure $
      case result of
        Left _ ->
          Left UnlockUnavailable
        Right (exitCode, stdoutText, stderrText) ->
          case exitCode of
            ExitSuccess ->
              Right (SessionKey (T.strip (T.pack stdoutText)))
            ExitFailure _ ->
              Left (UnlockFailed (T.pack stderrText))
  listItems (SessionKey rawSessionKey) = do
    baseEnv <- getEnvironment
    let command =
          (proc "bw" ["list", "items"])
            { env = Just (setEnvVar "BW_SESSION" (T.unpack rawSessionKey) baseEnv)
            }
    result <-
      try (readCreateProcessWithExitCode command "") ::
        IO (Either SomeException (ExitCode, String, String))
    pure $
      case result of
        Left _ ->
          Left ListItemsUnavailable
        Right (exitCode, stdoutText, stderrText) ->
          case exitCode of
            ExitSuccess ->
              case decodeItemSummaries (BS8.pack stdoutText) of
                Left decodeErr ->
                  Left (ListItemsFailed decodeErr)
                Right items ->
                  Right items
            ExitFailure _ ->
              Left (ListItemsFailed (T.pack stderrText))

decodeItemSummaries :: BS8.ByteString -> Either Text [ItemSummary]
decodeItemSummaries raw =
  case eitherDecodeStrict' raw of
    Left decodeErr ->
      Left (T.pack decodeErr)
    Right rawItems ->
      Right (extractLoginItems rawItems)

data BwItem = BwItem
  { bwItemId :: Text,
    bwItemName :: Text,
    bwItemLogin :: Maybe BwLogin
  }

instance FromJSON BwItem where
  parseJSON = withObject "BwItem" $ \obj ->
    BwItem <$> obj .: "id" <*> obj .: "name" <*> obj .:? "login"

data BwLogin = BwLogin
  { bwLoginUsername :: Maybe Text
  }

instance FromJSON BwLogin where
  parseJSON = withObject "BwLogin" $ \obj ->
    BwLogin <$> obj .:? "username"

extractLoginItems :: [BwItem] -> [ItemSummary]
extractLoginItems = foldr collect []
  where
    collect item acc =
      case bwItemLogin item of
        Nothing -> acc
        Just login ->
          ItemSummary
            (bwItemId item)
            (bwItemName item)
            (maybe "" id (bwLoginUsername login)) : acc

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars
