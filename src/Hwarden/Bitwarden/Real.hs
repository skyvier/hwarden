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
    ListItemsError (..),
    UnlockError (..),
    extractLoginItems
  )
import Hwarden.Logging (logInfo)
import Hwarden.Types (Password (Password), SessionKey (SessionKey), Username (Username))
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
  bitwardenCliAppDataDir :: r -> FilePath
  bitwardenServerUrl :: r -> Text

instance
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  Bitwarden (RealBitwardenT r m)
  where
  unlock (Username email) (Password password) = RealBitwardenT $ do
    katipAddContext (sl "email" email) $
      logInfo "running bw login"
    let args = [T.unpack email, T.unpack password, "--raw"]
    command <- isolatedBwProcess ("login" : args)
    handleCheckedCommand
      (runCommand command)
      UnlockUnavailable
      (Right . SessionKey . T.strip . T.pack)
      (UnlockFailed . T.pack)

  listItems (SessionKey rawSessionKey) = RealBitwardenT $ do
    logInfo "running bw list items"
    command <- authenticatedBwProcess (SessionKey rawSessionKey) ["list", "items"]
    handleCheckedByteCommand
      (readProcessBytes command)
      ListItemsUnavailable
      ( \stdoutBytes -> do
          bwItems <-
            first (ListItemsFailed . T.pack) $
              eitherDecodeStrict stdoutBytes
          pure (extractLoginItems bwItems)
      )
      (ListItemsFailed . T.pack . BS8.unpack)

configureServer ::
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m (Either Text ())
configureServer = do
  serverUrl <- asks bitwardenServerUrl
  logInfo "running bw config server"
  command <- isolatedBwProcess ["config", "server", T.unpack serverUrl]
  handleCheckedCommand
    (runCommand command)
    "bw config server failed"
    (const (Right ()))
    sanitizeCommandFailure

isolatedBwProcess ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  [String] ->
  m CreateProcess
isolatedBwProcess args = do
  isolatedEnv <- isolatedBwEnv
  pure (proc "bw" args) {env = Just isolatedEnv}

authenticatedBwProcess ::
  (MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  SessionKey ->
  [String] ->
  m CreateProcess
authenticatedBwProcess (SessionKey rawSessionKey) args = do
  isolatedEnv <- isolatedBwEnv
  pure
    (proc "bw" args)
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

sanitizeCommandFailure :: String -> Text
sanitizeCommandFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw config server failed" else trimmed

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars

readProcessBytes :: CreateProcess -> IO (ExitCode, BS.ByteString, BS.ByteString)
readProcessBytes command = do
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
