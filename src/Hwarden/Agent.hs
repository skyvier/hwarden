{-# LANGUAGE OverloadedStrings #-}

module Hwarden.Agent
  ( Bitwarden (..),
    AgentState (..),
    Decision (..),
    ItemSummary (..),
    ListItemsError (..),
    Password (..),
    Request (..),
    Response (..),
    SessionKey (..),
    UnlockError (..),
    Username (..),
    cleanupSocket,
    decide,
    handleListItems,
    handleUnlock,
    handleRequest,
    handleRequestWith,
    prepareSocketDir,
    removeExistingSocket,
    runAgent,
    sanitizeUnlockError
  )
where

import UnliftIO.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
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
import Hwarden.Bitwarden (Bitwarden (listItems, unlock), ListItemsError (..), UnlockError (..))
import Hwarden.Bitwarden.Real (RealBitwardenT (unrealBitwarden), configureServer)
import Hwarden.Logging (logInfo)
import Hwarden.Socket (recvAll)
import Hwarden.Types (ItemSummary (..), Password (..), SessionKey (..), Username (..))
import Hwarden.App (AgentT, runAgentT, Env(..), initAgentEnv)
import Katip
  ( KatipContext,
    Namespace,
    closeScribes,
    katipAddContext,
    katipAddNamespace,
    sl
  )
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
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

data Request
  = UnlockRequest Username Password
  | Status
  | ListItems
  | UnknownRequest
  deriving (Eq, Show)

data Response
  = Success Text
  | ItemList [ItemSummary]
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
      "list-items" -> pure ListItems
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
  toJSON ListItems =
    object
      [ "cmd" .= ("list-items" :: Text)
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
  toJSON (ItemList items) =
    object
      [ "ok" .= True,
        "items" .= items
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
      then (ItemList <$> obj .: "items") <|> (Success <$> obj .: "message")
      else Failure <$> obj .: "error"

runAgent :: IO ()
runAgent = do
  runtimeDir <- requireRuntimeDir
  env <- initAgentEnv runtimeDir
  let socketDir = runtimeDir </> "hwarden"
      socketPath = socketDir </> "agent.sock"
      bitwardenCliAppDataDir = envBitwardenCliAppDataDir env

  prepareSocketDir socketDir
  prepareSocketDir bitwardenCliAppDataDir
  removeExistingSocket socketPath
  bootstrapBitwardenCli env

  agentStateVar <- newMVar Locked
  sock <- socket AF_UNIX Stream defaultProtocol
  finally
    (do
        bind sock (SockAddrUnix socketPath)
        listen sock maxListenQueue
        forever $ do
          (conn, _) <- accept sock
          handleConnection env agentStateVar conn)
    (close sock `finally` (cleanupSocket socketPath `finally` closeScribes (envLogEnv env)))

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

bootstrapBitwardenCli :: Env -> IO ()
bootstrapBitwardenCli env = do
  result <- runAgentT env (unrealBitwarden configureServer)
  case result of
    Left err -> die ("failed to configure bitwarden server: " <> T.unpack err)
    Right () -> pure ()

handleConnection :: Env -> MVar AgentState -> Socket -> IO ()
handleConnection agentEnv agentStateVar conn =
  finally
    (runAgentT agentEnv $
        katipAddNamespace socketNamespace $ do
        traceId <- liftIO generateTraceId
        katipAddContext (sl "trace_id" traceId) $ do
          raw <- liftIO (recvAll conn)
          response <-
            case eitherDecodeStrict' raw of
              Left err -> do
                logRequestDecodeFailure err
                pure (Failure (T.pack err))
              Right request -> do
                logRequestReceived request
                handleRequest agentStateVar request
          liftIO (NBS.sendAll conn (LBS.toStrict (Aeson.encode response)))
          logResponseSent response)
    (close conn)

handleRequest :: MVar AgentState -> Request -> AgentT Response
handleRequest agentStateVar request =
  modifyMVar agentStateVar $ handleRequestWith request


handleRequestWith :: Bitwarden m => Request -> AgentState -> m (AgentState, Response)
handleRequestWith request agentState =
  case decide request agentState of
    Unlock username password -> 
      handleUnlock username password
    ListItemsAction sessionKey ->
      handleListItems sessionKey agentState
    Reply response -> pure (agentState, response)

data Decision 
  = Unlock Username Password
  | ListItemsAction SessionKey
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
decide ListItems agentState =
  case agentState of
    Locked -> Reply (Failure "locked")
    Unlocked sessionKey -> ListItemsAction sessionKey
decide UnknownRequest _ = Reply (Failure "unknown request")

handleUnlock :: Bitwarden m => Username -> Password -> m (AgentState, Response)
handleUnlock email password = do
  result <- unlock email password
  case result of
    Left UnlockUnavailable ->
      pure (Locked, Failure "bw login failed")
    Left (UnlockFailed err) ->
      pure (Locked, Failure (sanitizeUnlockError password err))
    Right sessionKey ->
      pure (Unlocked sessionKey, Success "unlocked")

handleListItems :: Bitwarden m => SessionKey -> AgentState -> m (AgentState, Response)
handleListItems sessionKey agentState = do
  result <- listItems sessionKey
  let response =
        case result of
          Left ListItemsUnavailable ->
            Failure "bw list items failed"
          Left (ListItemsFailed err) ->
            Failure (sanitizeListItemsFailure sessionKey err)
          Right items ->
            ItemList items
  pure (agentState, response)

sanitizeUnlockError :: Password -> Text -> Text
sanitizeUnlockError (Password password) err =
  let sanitized =
        if T.null password then err else T.replace password "<redacted>" err
      trimmed = T.strip sanitized
   in if T.null trimmed then "bw login failed" else trimmed

sanitizeListItemsFailure :: SessionKey -> Text -> Text
sanitizeListItemsFailure (SessionKey sessionKey) err =
  let sanitized =
        if T.null sessionKey then err else T.replace sessionKey "<redacted>" err
      trimmed = T.strip sanitized
   in if T.null trimmed then "bw list items failed" else trimmed

generateTraceId :: IO Text
generateTraceId = UUID.toText <$> nextRandom

logRequestDecodeFailure :: KatipContext m => String -> m ()
logRequestDecodeFailure decodeErr =
  katipAddContext
    (sl "request" ("invalid-json" :: String) <> sl "decode_error" decodeErr)
    (logInfo "received request")

logRequestReceived :: KatipContext m => Request -> m ()
logRequestReceived request =
  katipAddContext
    (sl "request" (show request))
    (logInfo "received request")

logResponseSent :: KatipContext m => Response -> m ()
logResponseSent response =
  katipAddContext
    (sl "response" (show response))
    (logInfo "sent response")

socketNamespace :: Namespace
socketNamespace = "socket"
