{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Agent
  ( Bitwarden (..),
    Password (..),
    Request (..),
    Response (..),
    SessionKey (..),
    UnlockError (..),
    Username (..),
    cleanupSocket,
    handleRequest,
    handleRequestWith,
    prepareSocketDir,
    removeExistingSocket,
    runAgent,
    sanitizeError
  )
where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Exception (finally)
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
import Hwarden.Bitwarden (Bitwarden (unlock), UnlockError (..))
import Hwarden.Socket (recvAll)
import Hwarden.Types (Password (..), SessionKey (..), Username (..))
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
import System.Exit (die)
import System.FilePath ((</>))
import System.Posix.Files (ownerModes, setFileMode)

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
handleRequest sessionVar =
  handleRequestWith storeSession
  where
    storeSession sessionKey = void (swapMVar sessionVar (Just sessionKey))

handleRequestWith :: Bitwarden m => (SessionKey -> m ()) -> Request -> m Response
handleRequestWith storeSession request =
  case request of
    UnlockRequest email password -> handleUnlock storeSession email password
    UnknownRequest -> pure (Failure "unknown command")

handleUnlock :: Bitwarden m => (SessionKey -> m ()) -> Username -> Password -> m Response
handleUnlock storeSession email password = do
  result <- unlock email password
  case result of
    Left UnlockUnavailable ->
      pure (Failure "bw login failed")
    Left (UnlockFailed err) ->
      pure (Failure (sanitizeError password err))
    Right sessionKey -> do
      storeSession sessionKey
      pure (Success "unlocked")

sanitizeError :: Password -> Text -> Text
sanitizeError (Password password) err =
  let trimmed = T.strip (T.replace password "<redacted>" err)
   in if T.null trimmed then "bw login failed" else trimmed
