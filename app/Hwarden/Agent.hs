{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Agent
  ( Password (..),
    Request (..),
    Response (..),
    SessionKey (..),
    Username (..),
    cleanupSocket,
    prepareSocketDir,
    removeExistingSocket,
    runAgent,
    sanitizeError
  )
where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, void, when)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    eitherDecodeStrict',
    object,
    withObject,
    (.:),
    (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Hwarden.Socket (recvAll)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    maxListenQueue,
    socket
  )
import qualified Network.Socket.ByteString as NBS
import System.Directory
  ( createDirectoryIfMissing,
    doesPathExist,
    removePathForcibly
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), die)
import System.FilePath ((</>))
import System.Posix.Files (ownerModes, setFileMode)
import System.Process (readProcessWithExitCode)

newtype SessionKey = SessionKey Text
  deriving (Eq)

instance Show SessionKey where
  show _ = "[REDACTED]"

newtype Username = Username Text
  deriving (Eq, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show _ = "[REDACTED]"

data Request
  = UnlockRequest Username Password
  | UnknownRequest
  deriving (Eq, Show)

data Response
  = Success Text
  | Failure Text
  deriving (Eq, Show)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    cmd <- obj .: "cmd"
    case (cmd :: Text) of
      "unlock" -> UnlockRequest <$> (Username <$> obj .: "email") <*> (Password <$> obj .: "password")
      _ -> pure UnknownRequest

instance ToJSON Response where
  toJSON (Success message) =
    object
      [ "ok" .= True,
        "message" .= message
      ]
  toJSON (Failure err) =
    object
      [ "ok" .= False,
        "error" .= err
      ]

runAgent :: IO ()
runAgent = do
  runtimeDir <- requireRuntimeDir
  let socketDir = runtimeDir </> "hwarden"
      socketPath = socketDir </> "agent.sock"

  prepareSocketDir socketDir
  removeExistingSocket socketPath

  sessionVar <- newMVar Nothing
  sock <- socket AF_UNIX Stream defaultProtocol
  finally
    (do
        bind sock (SockAddrUnix socketPath)
        listen sock maxListenQueue
        forever $ do
          (conn, _) <- accept sock
          handleConnection sessionVar conn)
    (close sock `finally` cleanupSocket socketPath)

requireRuntimeDir :: IO FilePath
requireRuntimeDir = do
  runtimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  case runtimeDir of
    Just path -> pure path
    Nothing -> die "XDG_RUNTIME_DIR is not set"

prepareSocketDir :: FilePath -> IO ()
prepareSocketDir socketDir = do
  createDirectoryIfMissing True socketDir
  setFileMode socketDir ownerModes

removeExistingSocket :: FilePath -> IO ()
removeExistingSocket socketPath = do
  exists <- doesPathExist socketPath
  when exists $ removePathForcibly socketPath

cleanupSocket :: FilePath -> IO ()
cleanupSocket socketPath = removeExistingSocket socketPath

handleConnection :: MVar (Maybe SessionKey) -> Socket -> IO ()
handleConnection sessionVar conn =
  finally
    (do
        raw <- recvAll conn
        response <-
          case eitherDecodeStrict' raw of
            Left err -> pure (Failure (T.pack err))
            Right request -> handleRequest sessionVar request
        NBS.sendAll conn (LBS.toStrict (Aeson.encode response)))
    (close conn)

handleRequest :: MVar (Maybe SessionKey) -> Request -> IO Response
handleRequest sessionVar request =
  case request of
    UnlockRequest email password -> unlock sessionVar email password
    UnknownRequest -> pure (Failure "unknown command")

unlock :: MVar (Maybe SessionKey) -> Username -> Password -> IO Response
unlock sessionVar (Username email) password@(Password passwordText) = do
  let args = [T.unpack email, T.unpack passwordText, "--raw"]
  result <-
    try (readProcessWithExitCode "bw" ("login" : args) "") ::
      IO (Either SomeException (ExitCode, String, String))
  case result of
    Left _ ->
      pure (Failure "bw login failed")
    Right (exitCode, stdoutText, stderrText) ->
      case exitCode of
        ExitSuccess -> do
          let sessionKey = SessionKey (T.strip (T.pack stdoutText))
          void (swapMVar sessionVar (Just sessionKey))
          pure (Success "unlocked")
        ExitFailure _ ->
          pure (Failure (sanitizeError password (T.pack stderrText)))

sanitizeError :: Password -> Text -> Text
sanitizeError (Password password) err =
  let trimmed = T.strip (T.replace password "<redacted>" err)
   in if T.null trimmed then "bw login failed" else trimmed
