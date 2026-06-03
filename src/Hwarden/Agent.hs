{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Hwarden.Agent
  ( Bitwarden (..),
    AgentState (..),
    FailureMessage (..),
    CacheAgeSeconds (..),
    CacheFillFailure (..),
    CacheEntry (..),
    ItemCacheState (..),
    LatestRefreshStatus (..),
    Effect (..),
    Decision (..),
    GetPasswordError (..),
    ItemSummary (..),
    ListItemsError (..),
    LoginItemId (..),
    Password (..),
    PasswordValue (..),
    Request (..),
    Command (..),
    fromCommandIdentifier,
    toCommandIdentifier,
    Response,
    SessionKey (..),
    UnlockError (..),
    Username (..),
    cleanupSocket,
    decide,
    handleGetPassword,
    handleListItems,
    handleRefreshResult,
    handleUnlock,
    handleRequest,
    handleRequestWith,
    failureResponse,
    itemListResponse,
    passwordResultResponse,
    prepareRuntimeDir,
    removeExistingSocket,
    responseErrorText,
    responseIsFailure,
    responseItems,
    responsePasswordResult,
    runAgent,
    sanitizeUnlockError,
    successResponse
  )
where

import UnliftIO.MVar (MVar, modifyMVar, newMVar)
import Control.Monad.Time (MonadTime, currentTime)
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when)
import Control.Monad.Reader (asks)
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
import GHC.Generics (Generic)
import Hwarden.Bitwarden
  ( Bitwarden (getPassword, unlock),
    GetPasswordError (..),
    ListItemsError (..),
    UnlockError (..),
  )
import Hwarden.Bitwarden.Real (configureServer)
import Hwarden.Cache
  ( CacheAgeSeconds (..),
    CacheEntry (..),
    CacheFillFailure (..),
    ItemCacheState (..),
    LatestRefreshStatus (..),
    buildInitialCacheState,
    cacheAgeSeconds,
    refreshCacheEntry,
    updateItemCacheState
  )
import Hwarden.Logging (MonadLog, logInfoF, logInfoS)
import Hwarden.Response
  ( FailureMessage (..),
    Response,
    failureResponse,
    itemListResponse,
    passwordResultResponse,
    responseErrorText,
    responseIsFailure,
    responseItems,
    responsePasswordResult,
    successResponse
  )
import qualified Hwarden.Runtime as Runtime
import Hwarden.Socket (recvAll)
import Hwarden.Types (ItemSummary (..), LoginItemId (..), Password (..), PasswordValue (..), SessionKey (..), Username (..))
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
import System.Posix.Files (ownerModes, setFileMode)
import Test.QuickCheck (elements, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Instances.Text ()
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified UnliftIO.Concurrent as Concurrent
import Hwarden.Sanitize
  ( sanitizeGetPasswordFailure,
    sanitizeUnlockError,
    trustStaticText
  )

data Request
  = UnlockRequest Username Password
  | Status
  | ListItems
  | GetPasswordRequest LoginItemId
  | UnknownRequest
  deriving (Eq, Generic)

instance Show Request where
  show (UnlockRequest username _) = 
    "unlock (" <> show username <> ")"
  show Status = "status"
  show ListItems = "list-items"
  show (GetPasswordRequest loginItemId) = 
    "get-password (" <> show loginItemId <> ")"
  show UnknownRequest = "unknown-request"

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    cmd <- obj .: "cmd"
    case fromCommandIdentifier (cmd :: Text) of
      Just UnlockCommand -> 
        UnlockRequest <$> (Username <$> obj .: "email") <*> (Password <$> obj .: "password")
      Just StatusCommand -> 
        pure Status
      Just ListItemsCommand -> 
        pure ListItems
      Just GetPasswordCommand -> 
        GetPasswordRequest . LoginItemId <$> obj .: "id"
      Nothing -> 
        pure UnknownRequest

instance ToJSON Request where
  toJSON (UnlockRequest (Username email) (Password password)) =
    object
      [ "cmd" .= toCommandIdentifier UnlockCommand,
        "email" .= email,
        "password" .= password
      ]
  toJSON Status =
    object
      [ "cmd" .= toCommandIdentifier StatusCommand
      ]
  toJSON ListItems =
    object
      [ "cmd" .= toCommandIdentifier ListItemsCommand 
      ]
  toJSON (GetPasswordRequest (LoginItemId passwordItemId)) =
    object
      [ "cmd" .= toCommandIdentifier GetPasswordCommand,
        "id" .= passwordItemId
      ]
  toJSON UnknownRequest =
    object
      [ "cmd" .= ("unknown" :: Text)
      ]

instance Arbitrary Request where
  arbitrary =
    oneof
      [ UnlockRequest <$> arbitrary <*> arbitrary,
        pure Status,
        pure ListItems,
        GetPasswordRequest <$> arbitrary,
        pure UnknownRequest
      ]
  shrink = genericShrink

data Command 
  = UnlockCommand
  | StatusCommand
  | ListItemsCommand
  | GetPasswordCommand 
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Command where
  arbitrary = elements [minBound..maxBound]
  shrink _ = []

-- TODO: write roundtrip tests for these

toCommandIdentifier :: Command -> Text
toCommandIdentifier UnlockCommand = "unlock"
toCommandIdentifier StatusCommand = "status"
toCommandIdentifier ListItemsCommand = "list-items"
toCommandIdentifier GetPasswordCommand = "get-password"

fromCommandIdentifier :: Text -> Maybe Command
fromCommandIdentifier "unlock" = Just UnlockCommand
fromCommandIdentifier "status" = Just StatusCommand
fromCommandIdentifier "list-items" = Just ListItemsCommand
fromCommandIdentifier "get-password" = Just GetPasswordCommand
fromCommandIdentifier _ = Nothing

data AgentState
  = Locked
  | Unlocked SessionKey ItemCacheState
  deriving (Eq, Show, Generic)

instance Arbitrary AgentState where
  arbitrary = do
    maybeSessionKey <- arbitrary
    case maybeSessionKey of
      Nothing -> pure Locked
      Just sessionKey -> Unlocked sessionKey <$> arbitrary
  shrink = genericShrink

runAgent :: IO ()
runAgent = do
  runtimeDir <- requireRuntimeDir
  paths <- either die pure (Runtime.deriveAgentPaths runtimeDir)
  env <- initAgentEnv runtimeDir

  prepareRuntimeDir (Runtime.socketDir paths)
  prepareRuntimeDir (Runtime.bitwardenCliAppDataDir paths)
  removeExistingSocket (Runtime.socketPath paths)
  bootstrapBitwardenCli env

  agentStateVar <- newMVar Locked
  sock <- socket AF_UNIX Stream defaultProtocol
  finally
    (do
        bind sock (SockAddrUnix (Runtime.socketPath paths))
        listen sock maxListenQueue
        forever $ do
          (conn, _) <- accept sock
          handleConnection env agentStateVar conn)
    (close sock `finally` (cleanupSocket (Runtime.socketPath paths) `finally` closeScribes (envLogEnv env)))

requireRuntimeDir :: IO FilePath
requireRuntimeDir = do
  runtimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  case runtimeDir of
    Just path -> pure path
    Nothing -> die "XDG_RUNTIME_DIR is not set"

prepareRuntimeDir :: FilePath -> IO ()
prepareRuntimeDir runtimeDir = do
  createDirectoryIfMissing True runtimeDir
  setFileMode runtimeDir ownerModes

removeExistingSocket :: FilePath -> IO ()
removeExistingSocket socketPath = do
  exists <- doesPathExist socketPath
  when exists $ removePathForcibly socketPath

cleanupSocket :: FilePath -> IO ()
cleanupSocket socketPath = removeExistingSocket socketPath

bootstrapBitwardenCli :: Env -> IO ()
bootstrapBitwardenCli env = do
  result <- runAgentT env configureServer
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
                pure (failureResponse (StaticFailure (trustStaticText (T.pack err))))
              Right request -> do
                logRequestReceived request
                handleRequest agentStateVar request
          liftIO (NBS.sendAll conn (LBS.toStrict (Aeson.encode response)))
          logResponseSent response)
    (close conn)

handleRequest :: MVar AgentState -> Request -> AgentT Response
handleRequest agentStateVar request = do
  (response, effects) <-
    modifyMVar agentStateVar $ \agentState -> do
      (newState, response, effects) <- handleRequestWith request agentState
      pure (newState, (response, effects))
  mapM_ (runEffect agentStateVar) effects
  pure response


handleRequestWith :: (Bitwarden m, MonadTime m) => Request -> AgentState -> m (AgentState, Response, [Effect])
handleRequestWith request agentState =
  case decide request agentState of
    Unlock username password ->
      handleUnlock username password
    ListItemsAction cacheEntry ->
      handleListItems cacheEntry agentState
    GetPasswordAction sessionKey loginItemId ->
      handleGetPassword sessionKey loginItemId agentState
    Reply response -> pure (agentState, response, [])

data Effect
  = StartCacheRefreshLoop SessionKey
  deriving (Eq, Show)

data Decision 
  = Unlock Username Password
  | ListItemsAction CacheEntry
  | GetPasswordAction SessionKey LoginItemId
  | Reply Response
  deriving (Eq, Show)

decide :: Request -> AgentState -> Decision
decide (UnlockRequest username password) agentState =
  case agentState of
    Unlocked _ _ -> Reply (successResponse "already unlocked")
    Locked -> Unlock username password
decide Status agentState =
  case agentState of
    Locked -> Reply (successResponse "locked")
    Unlocked _ _ -> Reply (successResponse "unlocked")
decide ListItems agentState =
  case agentState of
    Locked -> Reply (failureResponse "locked")
    Unlocked _ (CacheReady cacheEntry _) -> ListItemsAction cacheEntry
    Unlocked _ _ -> Reply (failureResponse "item cache unavailable")
decide (GetPasswordRequest loginItemId) agentState =
  case agentState of
    Locked -> Reply (failureResponse "locked")
    Unlocked sessionKey _ -> GetPasswordAction sessionKey loginItemId
decide UnknownRequest _ = Reply (failureResponse "unknown request")

handleUnlock :: (Bitwarden m, MonadTime m) => Username -> Password -> m (AgentState, Response, [Effect])
handleUnlock email password = do
  result <- unlock email password
  case result of
    Left UnlockUnavailable ->
      pure (Locked, failureResponse "bw login failed", [])
    Left CodeRequired ->
      pure (Locked, failureResponse "two-factor code required; run scripts/hwarden-first-login", [])
    Left (UnlockFailed err) ->
      pure (Locked, unlockFailureResponse password err, [])
    Right sessionKey -> do
      cacheState <- buildInitialCacheState sessionKey
      pure
        ( Unlocked sessionKey cacheState,
          successResponse "unlocked",
          [StartCacheRefreshLoop sessionKey]
        )

handleListItems :: MonadTime m => CacheEntry -> AgentState -> m (AgentState, Response, [Effect])
handleListItems cacheEntry agentState = do
  now <- currentTime
  pure (agentState, itemListResponse (cacheEntryItems cacheEntry) (cacheAgeSeconds now cacheEntry), [])

runEffect :: MVar AgentState -> Effect -> AgentT ()
runEffect agentStateVar effect =
  case effect of
    StartCacheRefreshLoop sessionKey ->
      startRefreshLoop agentStateVar sessionKey

startRefreshLoop :: MVar AgentState -> SessionKey -> AgentT ()
startRefreshLoop agentStateVar sessionKey = do
  refreshIntervalMicroseconds <-
    (* 1000000) <$> asks envCacheRefreshIntervalSeconds
  katipAddNamespace cacheRefreshNamespace $ do
    logInfoS @"starting item cache refresh loop" @AgentT
    void $
      Concurrent.forkIO (refreshLoop refreshIntervalMicroseconds)
  where
    refreshLoop refreshIntervalMicroseconds = do
      Concurrent.threadDelay refreshIntervalMicroseconds
      logInfoS @"running item cache refresh" @AgentT
      refreshResult <- refreshCacheEntry sessionKey
      shouldContinue <-
        modifyMVar agentStateVar $
          \agentState -> pure $ handleRefreshResult sessionKey refreshResult agentState
      logRefreshResult refreshResult shouldContinue
      when shouldContinue $
        refreshLoop refreshIntervalMicroseconds

handleRefreshResult :: SessionKey -> Either CacheFillFailure CacheEntry -> AgentState -> (AgentState, Bool)
handleRefreshResult sessionKey refreshResult agentState =
  case classifyRefreshOwnership sessionKey agentState of
    RefreshWorkerOwnsSession currentSessionKey itemCacheState ->
      ( Unlocked currentSessionKey (updateItemCacheState itemCacheState refreshResult),
        True
      )
    RefreshWorkerNoLongerOwnsSession ->
      (agentState, False)

data RefreshOwnership
  = RefreshWorkerOwnsSession SessionKey ItemCacheState
  | RefreshWorkerNoLongerOwnsSession

classifyRefreshOwnership :: SessionKey -> AgentState -> RefreshOwnership
classifyRefreshOwnership sessionKey agentState =
  case agentState of
    Unlocked currentSessionKey itemCacheState
      | currentSessionKey == sessionKey ->
          RefreshWorkerOwnsSession currentSessionKey itemCacheState
    _ -> RefreshWorkerNoLongerOwnsSession

handleGetPassword :: Bitwarden m => SessionKey -> LoginItemId -> AgentState -> m (AgentState, Response, [Effect])
handleGetPassword sessionKey loginItemId agentState = do
  result <- getPassword sessionKey loginItemId
  let response =
        case result of
          Left GetPasswordUnavailable ->
            failureResponse "bw get password failed"
          Left (GetPasswordFailed err) ->
            getPasswordFailureResponse sessionKey err
          Right password ->
            passwordResultResponse loginItemId password
  pure (agentState, response, [])

unlockFailureResponse :: Password -> Text -> Response
unlockFailureResponse password err =
  failureResponse (PasswordSanitizedFailure (sanitizeUnlockError password err))

getPasswordFailureResponse :: SessionKey -> Text -> Response
getPasswordFailureResponse sessionKey err =
  failureResponse (SessionSanitizedFailure (sanitizeGetPasswordFailure sessionKey err))

logRefreshResult :: forall m. MonadLog m => Either CacheFillFailure CacheEntry -> Bool -> m ()
logRefreshResult refreshResult shouldContinue =
  case (refreshResult, shouldContinue) of
    (Right _, True) ->
      logInfoS @"item cache refresh succeeded" @m
    (Left CacheFillUnavailable, True) ->
      logInfoS @"item cache refresh failed: unavailable" @m
    (Left (CacheFillFailed err), True) ->
      logInfoF @"item cache refresh failed: %{SessionSanitized}" @m err
    (_, False) ->
      logInfoS @"stopping item cache refresh loop" @m

generateTraceId :: IO Text
generateTraceId = UUID.toText <$> nextRandom

logRequestDecodeFailure :: forall m. (KatipContext m, MonadLog m) => String -> m ()
logRequestDecodeFailure decodeErr =
  katipAddContext
    (sl "request" ("invalid-json" :: String) <> sl "decode_error" decodeErr)
    (logInfoS @"received request" @m)

logRequestReceived :: forall m. (KatipContext m, MonadLog m) => Request -> m ()
logRequestReceived request =
  katipAddContext
    (sl "request" (show request))
    (logInfoS @"received request" @m)

logResponseSent :: forall m. (KatipContext m, MonadLog m) => Response -> m ()
logResponseSent response =
  katipAddContext
    (sl "response" (show response))
    (logInfoS @"sent response" @m)

socketNamespace :: Namespace
socketNamespace = "socket"

cacheRefreshNamespace :: Namespace
cacheRefreshNamespace = "cache-refresh"
