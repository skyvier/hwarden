{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hwarden.Bitwarden.Real
  ( RealBitwardenT (..),
    HasBitwardenCliConfig (..),
    configureServer,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, try)
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
    UnlockError (..),
    extractLoginItems
  )
import Hwarden.Logging (logInfo)
import Hwarden.Types
  ( LoginItemId (LoginItemId),
    Password (Password),
    PasswordValue (PasswordValue),
    SessionKey (SessionKey),
    TwoFactorCode (TwoFactorCode),
    Username (Username)
  )
import Katip (Katip, KatipContext, katipAddContext, sl)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess (env, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getProcessExitCode,
    proc,
    readCreateProcessWithExitCode,
    terminateProcess,
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
  unlock (Username email) (Password password) maybeCode = RealBitwardenT $ do
    katipAddContext (sl "email" email) $
      logInfo "running bw login"
    let args =
          [T.unpack email, T.unpack password, "--raw"]
            <> maybe [] loginOtpArgs maybeCode
    command <- isolatedBwProcess ("login" : args)
    result <- liftIO (runLoginCommand maybeCode command)
    pure $
      case result of
        Left err -> Left err
        Right (exitCode, stdoutText, stderrText) ->
          case exitCode of
            ExitSuccess ->
              Right (SessionKey (T.strip (T.pack stdoutText)))
            ExitFailure _ ->
              Left (UnlockFailed (sanitizeUnlockFailure maybeCode stderrText))

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

runLoginCommand :: Maybe TwoFactorCode -> CreateProcess -> IO (Either UnlockError (ExitCode, String, String))
runLoginCommand maybeCode command = do
  timeoutMicros <- loginTimeoutMicros
  result <-
    ( try
        (do
          (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) <-
            createProcess
              command
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
          hClose stdinHandle
          stdoutVar <- newEmptyMVar
          stderrVar <- newEmptyMVar
          _ <- forkIO (readHandleText stdoutHandle stdoutVar)
          _ <- forkIO (readHandleText stderrHandle stderrVar)
          exitCode <- waitForExit timeoutMicros processHandle
          case exitCode of
            Nothing -> do
              terminateProcess processHandle
              terminated <- waitForExit timeoutMicros processHandle
              case terminated of
                Nothing ->
                  pure (Left (unlockTimeoutFailure maybeCode ""))
                Just terminatedExitCode -> do
                  _ <- takeMVar stdoutVar
                  stderrText <- takeMVar stderrVar
                  pure (Right (terminatedExitCode, "", stderrText))
            Just finishedExitCode -> do
              stdoutText <- takeMVar stdoutVar
              stderrText <- takeMVar stderrVar
              pure (Right (finishedExitCode, stdoutText, stderrText)))
      ) ::
        IO (Either SomeException (Either UnlockError (ExitCode, String, String)))
  pure $
    case result of
      Left (_ :: SomeException) -> Left UnlockUnavailable
      Right loginResult -> loginResult

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

loginOtpArgs :: TwoFactorCode -> [String]
loginOtpArgs (TwoFactorCode code) =
  ["--method", "1", "--code", T.unpack code]

loginTimeoutMicros :: IO Int
loginTimeoutMicros = do
  envVars <- getEnvironment
  pure $
    case lookup "HWARDEN_BW_LOGIN_TIMEOUT_MICROS" envVars >>= readMaybeInt of
      Just micros -> micros
      Nothing -> defaultLoginTimeoutMicros

defaultLoginTimeoutMicros :: Int
defaultLoginTimeoutMicros = 5 * 1000 * 1000

pollIntervalMicros :: Int
pollIntervalMicros = 50 * 1000

readHandleText :: Handle -> MVar String -> IO ()
readHandleText handle outputVar = do
  bytes <- BS.hGetContents handle
  _ <- evaluate (BS.length bytes)
  hClose handle
  putMVar outputVar (BS8.unpack bytes)

waitForExit :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitForExit timeoutMicros processHandle = go timeoutMicros
  where
    go remainingMicros = do
      exitCode <- getProcessExitCode processHandle
      case exitCode of
        Just code -> pure (Just code)
        Nothing
          | remainingMicros <= 0 -> pure Nothing
          | otherwise -> do
              threadDelay pollIntervalMicros
              go (remainingMicros - pollIntervalMicros)

unlockTimeoutFailure :: Maybe TwoFactorCode -> String -> UnlockError
unlockTimeoutFailure maybeCode stderrText =
  UnlockFailed $
    case maybeCode of
      Nothing -> "two-factor code required"
      Just _ -> fallbackUnlockFailure stderrText "bw login timed out"

sanitizeUnlockFailure :: Maybe TwoFactorCode -> String -> Text
sanitizeUnlockFailure maybeCode stderrText =
  case maybeCode of
    Nothing
      | looksLikeOtpRequired trimmed -> "two-factor code required"
      | T.null trimmed -> "two-factor code required"
      | otherwise -> trimmed
    Just _ -> fallbackUnlockFailure stderrText "bw login failed"
  where
    trimmed = T.strip (T.pack stderrText)

fallbackUnlockFailure :: String -> Text -> Text
fallbackUnlockFailure stderrText defaultMessage =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then defaultMessage else trimmed

looksLikeOtpRequired :: Text -> Bool
looksLikeOtpRequired stderrText =
  let lowered = T.toLower stderrText
   in or
        [ "two-factor" `T.isInfixOf` lowered,
          "two factor" `T.isInfixOf` lowered,
          "two-step" `T.isInfixOf` lowered,
          "verification code" `T.isInfixOf` lowered,
          "email verification" `T.isInfixOf` lowered
        ]

sanitizeCommandFailure :: String -> Text
sanitizeCommandFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw config server failed" else trimmed

sanitizeLogoutFailure :: String -> Text
sanitizeLogoutFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw logout failed" else trimmed

parsePasswordValue :: String -> Either GetPasswordError PasswordValue
parsePasswordValue stdoutText =
  let trimmed = T.strip (T.pack stdoutText)
   in if T.null trimmed
        then Left (GetPasswordFailed "password was empty")
        else Right (PasswordValue trimmed)

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars

readMaybeInt :: String -> Maybe Int
readMaybeInt value =
  case reads value of
    [(number, "")] -> Just number
    _ -> Nothing
