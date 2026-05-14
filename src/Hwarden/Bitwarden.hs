{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Bitwarden
  ( Bitwarden (..),
    ListItemsError (..),
    UnlockError (..),
    BwItem (..), 
    BwLogin (..),
    extractLoginItems
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Bifunctor (first)
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
            ExitSuccess -> do
              bwItems <- 
                first (ListItemsFailed . T.pack) $ 
                  eitherDecodeStrict (BS8.pack stdoutText)
              return $ extractLoginItems bwItems
            ExitFailure _ ->
              Left (ListItemsFailed (T.pack stderrText))

data BwItem = BwItem
  { bwItemId :: Text,
    bwItemName :: Text,
    bwItemLogin :: Maybe BwLogin
  }
  deriving (Eq, Show)

instance FromJSON BwItem where
  parseJSON = withObject "BwItem" $ \obj ->
    BwItem <$> obj .: "id" <*> obj .: "name" <*> obj .:? "login"

data BwLogin = BwLogin
  { bwLoginUsername :: Text
  }
  deriving (Eq, Show)

instance FromJSON BwLogin where
  parseJSON = withObject "BwLogin" $ \obj ->
    BwLogin <$> obj .: "username"

extractLoginItems :: [BwItem] -> [ItemSummary]
extractLoginItems = mapMaybe toItemSummary

toItemSummary :: BwItem -> Maybe ItemSummary
toItemSummary item =
  case bwItemLogin item of
    Nothing -> Nothing
    Just login ->
      Just
        ( ItemSummary
            (bwItemId item)
            (bwItemName item)
            (bwLoginUsername login)
        )

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars
