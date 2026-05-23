{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Hwarden.Bitwarden.Real
  ( RealBitwardenT (..),
    HasBitwardenCliConfig (..),
    configureServer,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Bitwarden
  ( Bitwarden (..),
    GetPasswordError (..),
    ListItemsError (..),
    SyncError (..),
    UnlockError (..),
    extractLoginItems
  )
import Hwarden.Logging (logInfo)
import Hwarden.Types
  ( LoginItemId (LoginItemId),
    Password (Password),
    PasswordValue (PasswordValue),
    SessionKey (SessionKey),
    Username (Username)
  )
import Katip (Katip, KatipContext, katipAddContext, sl)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hClose)
import System.Process
  ( CreateProcess (env, std_err, std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess
  )

newtype RealBitwardenT r m a = RealBitwardenT
  { unrealBitwarden :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, Katip, KatipContext, MonadReader r)

class HasBitwardenCliConfig r where
  bitwardenCliPath :: r -> FilePath
  bitwardenCliAppDataDir :: r -> FilePath
  bitwardenServerUrl :: r -> Text

instance
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  Bitwarden (RealBitwardenT r m)
  where
  unlock (Username email) (Password password) = RealBitwardenT $ do
    katipAddContext (sl "email" email) $
      logInfo "running bw login"
    let args =
          [ "login",
            "--nointeraction",
            T.unpack email,
            "--passwordenv",
            loginPasswordEnvVar,
            "--raw"
          ]
    command <- isolatedBwProcessWithEnv [(loginPasswordEnvVar, T.unpack password)] args
    handleCheckedCommand
      (runCommand command)
      UnlockUnavailable
      (Right . SessionKey . T.strip . T.pack)
      sanitizeUnlockFailure

  listItems (SessionKey rawSessionKey) = RealBitwardenT $ do
    logInfo "running bw list items"
    command <- authenticatedBwProcess (SessionKey rawSessionKey) ["list", "items"]
    handleCheckedByteCommand
      (runProcessBytes command)
      ListItemsUnavailable
      ( \stdoutBytes -> do
          bwItems <-
            first (ListItemsFailed . T.pack) $
              eitherDecodeStrict stdoutBytes
          pure (extractLoginItems bwItems)
      )
      (ListItemsFailed . T.pack . BS8.unpack)

  sync (SessionKey rawSessionKey) = RealBitwardenT $ do
    logInfo "running bw sync"
    command <- authenticatedBwProcess (SessionKey rawSessionKey) ["sync"]
    handleCheckedCommand
      (runCommand command)
      SyncUnavailable
      (const (Right ()))
      sanitizeSyncFailure

  getPassword (SessionKey rawSessionKey) (LoginItemId itemId) = RealBitwardenT $ do
    logInfo "running bw get password"
    command <- authenticatedBwProcess (SessionKey rawSessionKey) ["get", "password", T.unpack itemId]
    handleCheckedCommand
      (runCommand command)
      GetPasswordUnavailable
      parsePasswordValue
      (GetPasswordFailed . T.pack)

configureServer ::
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m (Either Text ())
configureServer = do
  bestEffortLogout
  serverUrl <- asks bitwardenServerUrl
  logInfo "running bw config server"
  command <- isolatedBwProcess ["config", "server", T.unpack serverUrl]
  handleCheckedCommand
    (runCommand command)
    "bw config server failed"
    (const (Right ()))
    sanitizeCommandFailure

bestEffortLogout ::
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m ()
bestEffortLogout = do
  logInfo "running bw logout"
  command <- isolatedBwProcess ["logout"]
  result <-
    handleCheckedCommand
      (runCommand command)
      "bw logout failed"
      (const (Right ()))
      sanitizeLogoutFailure
  case result of
    Left err ->
      logInfo ("bw logout failed; continuing startup: " <> err)
    Right () ->
      pure ()

isolatedBwProcess ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  [String] ->
  m CreateProcess
isolatedBwProcess = isolatedBwProcessWithEnv []

isolatedBwProcessWithEnv ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  [(String, String)] ->
  [String] ->
  m CreateProcess
isolatedBwProcessWithEnv extraEnv args = do
  cliPath <- asks bitwardenCliPath
  isolatedEnv <- isolatedBwEnv
  pure (proc cliPath args) {env = Just (foldr (uncurry setEnvVar) isolatedEnv extraEnv)}

authenticatedBwProcess ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  SessionKey ->
  [String] ->
  m CreateProcess
authenticatedBwProcess (SessionKey rawSessionKey) args = do
  cliPath <- asks bitwardenCliPath
  isolatedEnv <- isolatedBwEnv
  pure
    (proc cliPath args)
      { env = Just (setEnvVar "BW_SESSION" (T.unpack rawSessionKey) isolatedEnv)
      }

isolatedBwEnv ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m [(String, String)]
isolatedBwEnv = do
  appDataDir <- asks bitwardenCliAppDataDir
  baseEnv <- liftIO getEnvironment
  pure (setEnvVar "BITWARDENCLI_APPDATA_DIR" appDataDir baseEnv)

runCommand :: CreateProcess -> IO (ExitCode, String, String)
runCommand command = readCreateProcessWithExitCode command ""

runProcessBytes :: CreateProcess -> IO (ExitCode, BS.ByteString, BS.ByteString)
runProcessBytes command = do
  (Nothing, Just stdoutHandle, Just stderrHandle, processHandle) <-
    createProcess
      command
        { std_out = CreatePipe,
          std_err = CreatePipe
        }
  stdoutBytes <- BS.hGetContents stdoutHandle
  stderrBytes <- BS.hGetContents stderrHandle
  exitCode <- waitForProcess processHandle
  hClose stdoutHandle
  hClose stderrHandle
  pure (exitCode, stdoutBytes, stderrBytes)

handleCheckedCommand ::
  MonadIO m =>
  IO (ExitCode, String, String) ->
  err ->
  (String -> Either err a) ->
  (String -> err) ->
  m (Either err a)
handleCheckedCommand action unavailable handleSuccess handleFailure = do
  result <-
    liftIO
      (try action :: IO (Either SomeException (ExitCode, String, String)))
  pure $
    case result of
      Left _ ->
        Left unavailable
      Right (exitCode, stdoutText, stderrText) ->
        case exitCode of
          ExitSuccess -> handleSuccess stdoutText
          ExitFailure _ -> Left (handleFailure stderrText)

handleCheckedByteCommand ::
  MonadIO m =>
  IO (ExitCode, BS.ByteString, BS.ByteString) ->
  err ->
  (BS.ByteString -> Either err a) ->
  (BS.ByteString -> err) ->
  m (Either err a)
handleCheckedByteCommand action unavailable handleSuccess handleFailure = do
  result <-
    liftIO
      (try action :: IO (Either SomeException (ExitCode, BS.ByteString, BS.ByteString)))
  pure $
    case result of
      Left _ ->
        Left unavailable
      Right (exitCode, stdoutBytes, stderrBytes) ->
        case exitCode of
          ExitSuccess -> handleSuccess stdoutBytes
          ExitFailure _ -> Left (handleFailure stderrBytes)

sanitizeUnlockFailure :: String -> UnlockError
sanitizeUnlockFailure stderrText
  | "Code is required" `T.isInfixOf` trimmed = CodeRequired
  | T.null trimmed = UnlockFailed "bw login failed"
  | otherwise = UnlockFailed trimmed
  where
    trimmed = T.strip (T.pack stderrText)

sanitizeCommandFailure :: String -> Text
sanitizeCommandFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw config server failed" else trimmed

sanitizeLogoutFailure :: String -> Text
sanitizeLogoutFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw logout failed" else trimmed

sanitizeSyncFailure :: String -> SyncError
sanitizeSyncFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then SyncFailed "bw sync failed" else SyncFailed trimmed

loginPasswordEnvVar :: String
loginPasswordEnvVar = "HWARDEN_BW_PASSWORD"

parsePasswordValue :: String -> Either GetPasswordError PasswordValue
parsePasswordValue stdoutText =
  let trimmed = T.strip (T.pack stdoutText)
   in if T.null trimmed
        then Left (GetPasswordFailed "password was empty")
        else Right (PasswordValue trimmed)

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars
