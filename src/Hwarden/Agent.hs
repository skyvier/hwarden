{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Agent
  ( Bitwarden (..),
    AgentState (..),
    Decision (..),
    Password (..),
    Request (..),
    Response (..),
    SessionKey (..),
    UnlockError (..),
    Username (..),
    cleanupSocket,
    decide,
    handleUnlock,
    handleRequest,
    handleRequestWith,
    prepareSocketDir,
    removeExistingSocket,
    runAgent,
    sanitizeError
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally)
import Control.Monad (forever, when)
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
import Test.QuickCheck (Arbitrary (arbitrary))

data Request
  = UnlockRequest Username Password
  | Status
  | UnknownRequest
  deriving (Eq, Show)

data Response
  = Success Text
  | Failure Text
  deriving (Eq, Show)

data AgentState
  = Locked
  | Unlocked SessionKey
  deriving (Eq, Show)

instance Arbitrary AgentState where
  arbitrary =
    propertyState <$> arbitrary
    where
      propertyState Nothing = Locked
      propertyState (Just sessionKey) = Unlocked sessionKey

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    cmd <- obj .: "cmd"
    case (cmd :: Text) of
      "unlock" -> UnlockRequest <$> (Username <$> obj .: "email") <*> (Password <$> obj .: "password")
      "status" -> pure Status
      _ -> pure UnknownRequest

instance ToJSON Request where
  toJSON (UnlockRequest (Username email) (Password password)) =
    object
      [ "cmd" .= ("unlock" :: Text),
        "email" .= email,
        "password" .= password
      ]
  toJSON Status =
    object
      [ "cmd" .= ("status" :: Text)
      ]
  toJSON UnknownRequest =
    object
      [ "cmd" .= ("unknown" :: Text)
      ]

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

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then Success <$> obj .: "message"
      else Failure <$> obj .: "error"

runAgent :: IO ()
runAgent = do
  runtimeDir <- requireRuntimeDir
  let socketDir = runtimeDir </> "hwarden"
      socketPath = socketDir </> "agent.sock"

  prepareSocketDir socketDir
  removeExistingSocket socketPath

  agentStateVar <- newMVar Locked
  sock <- socket AF_UNIX Stream defaultProtocol
  finally
    (do
        bind sock (SockAddrUnix socketPath)
        listen sock maxListenQueue
        forever $ do
          (conn, _) <- accept sock
          handleConnection agentStateVar conn)
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

handleConnection :: MVar AgentState -> Socket -> IO ()
handleConnection agentStateVar conn =
  finally
    (do
        raw <- recvAll conn
        response <-
          case eitherDecodeStrict' raw of
            Left err -> pure (Failure (T.pack err))
            Right request -> handleRequest agentStateVar request
        NBS.sendAll conn (LBS.toStrict (Aeson.encode response)))
    (close conn)

handleRequest :: MVar AgentState -> Request -> IO Response
handleRequest agentStateVar request =
  modifyMVar agentStateVar $ handleRequestWith request


handleRequestWith :: Bitwarden m => Request -> AgentState -> m (AgentState, Response)
handleRequestWith request agentState =
  case decide request agentState of
    Unlock username password -> 
      handleUnlock username password
    Reply response -> pure (agentState, response)

data Decision 
  = Unlock Username Password
  | Reply Response
  deriving (Eq, Show)

decide :: Request -> AgentState -> Decision
decide (UnlockRequest username password) agentState =
  case agentState of
    Unlocked _ -> Reply (Success "already unlocked")
    Locked -> Unlock username password
decide Status agentState =
  case agentState of
    Locked -> Reply (Success "locked")
    Unlocked _ -> Reply (Success "unlocked")
decide UnknownRequest _ = Reply (Failure "unknown request")

handleUnlock :: Bitwarden m => Username -> Password -> m (AgentState, Response)
handleUnlock email password = do
  result <- unlock email password
  case result of
    Left UnlockUnavailable ->
      pure (Locked, Failure "bw login failed")
    Left (UnlockFailed err) ->
      pure (Locked, Failure (sanitizeError password err))
    Right sessionKey ->
      pure (Unlocked sessionKey, Success "unlocked")

sanitizeError :: Password -> Text -> Text
sanitizeError (Password password) err =
  let trimmed = T.strip (T.replace password "<redacted>" err)
   in if T.null trimmed then "bw login failed" else trimmed
