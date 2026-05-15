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

newtype RealBitwardenT m a = RealBitwardenT
  { unrealBitwarden :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, Katip, KatipContext)

class Monad m => HasBitwardenCliConfig m where
  getBitwardenCliAppDataDir :: m FilePath
  getBitwardenServerUrl :: m Text

instance HasBitwardenCliConfig m => HasBitwardenCliConfig (RealBitwardenT m) where
  getBitwardenCliAppDataDir = RealBitwardenT getBitwardenCliAppDataDir
  getBitwardenServerUrl = RealBitwardenT getBitwardenServerUrl

instance (KatipContext m, MonadIO m, HasBitwardenCliConfig m) => Bitwarden (RealBitwardenT m) where
  unlock (Username email) (Password password) = RealBitwardenT $ do
    katipAddContext (sl "email" email) $
      logInfo "running bw login"
    let args = [T.unpack email, T.unpack password, "--raw"]
    command <- buildCommand (proc "bw" ("login" : args))
    result <-
      liftIO
        ( try (runCommand command) ::
            IO (Either SomeException (ExitCode, String, String))
        )
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

  listItems (SessionKey rawSessionKey) = RealBitwardenT $ do
    logInfo "running bw list items"
    command <-
      buildCommandWithExtraEnv
        [("BW_SESSION", T.unpack rawSessionKey)]
        (proc "bw" ["list", "items"])
    result <-
      liftIO
        ( try (readProcessBytes command) ::
            IO (Either SomeException (ExitCode, BS.ByteString, BS.ByteString))
        )
    case result of
      Left _ ->
        pure $ Left ListItemsUnavailable
      Right (exitCode, stdoutBytes, stderrBytes) ->
        case exitCode of
          ExitSuccess -> do
            pure $ do
              bwItems <-
                first (ListItemsFailed . T.pack) $
                  eitherDecodeStrict stdoutBytes
              pure (extractLoginItems bwItems)
          ExitFailure _ ->
            pure $ Left (ListItemsFailed (T.pack (BS8.unpack stderrBytes)))

configureServer :: (KatipContext m, MonadIO m, HasBitwardenCliConfig m) => m (Either Text ())
configureServer = do
  serverUrl <- getBitwardenServerUrl
  logInfo "running bw config server"
  command <- buildCommand (proc "bw" ["config", "server", T.unpack serverUrl])
  result <-
    liftIO
      ( try (runCommand command) ::
          IO (Either SomeException (ExitCode, String, String))
      )
  pure $
    case result of
      Left _ ->
        Left "bw config server failed"
      Right (exitCode, _, stderrText) ->
        case exitCode of
          ExitSuccess -> Right ()
          ExitFailure _ ->
            let trimmed = T.strip (T.pack stderrText)
             in Left (if T.null trimmed then "bw config server failed" else trimmed)

buildCommand :: (MonadIO m, HasBitwardenCliConfig m) => CreateProcess -> m CreateProcess
buildCommand = buildCommandWithExtraEnv []

buildCommandWithExtraEnv :: (MonadIO m, HasBitwardenCliConfig m) => [(String, String)] -> CreateProcess -> m CreateProcess
buildCommandWithExtraEnv extraEnv command = do
  appDataDir <- getBitwardenCliAppDataDir
  baseEnv <- liftIO getEnvironment
  pure
    command
      { env =
          Just
            (foldr (uncurry setEnvVar) (setEnvVar "BITWARDENCLI_APPDATA_DIR" appDataDir baseEnv) extraEnv)
      }

runCommand :: CreateProcess -> IO (ExitCode, String, String)
runCommand command = readCreateProcessWithExitCode command ""

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
