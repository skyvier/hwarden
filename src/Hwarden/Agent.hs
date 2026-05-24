{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
    Response,
    SessionKey (..),
    UnlockError (..),
    Username (..),
    cleanupSocket,
    decide,
    handleGetPassword,
    handleListItems,
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
    cacheAgeSeconds,
    cacheFillFailureFromListItemsError,
    sanitizeUnlockError,
    successResponse,
    updateItemCacheState
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
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hwarden.Bitwarden
  ( Bitwarden (getPassword, listItems, unlock),
    GetPasswordError (..),
    ListItemsError (..),
    UnlockError (..), SyncError (..), sync
  )
import Hwarden.Bitwarden.Real (configureServer)
import Hwarden.Logging (logInfo)
import Hwarden.Response
  ( CacheAgeSeconds (..),
    FailureMessage (..),
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
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified UnliftIO.Concurrent as Concurrent
import Control.Monad.Except (runExceptT, ExceptT (..))
import Data.Bifunctor (first)
import Hwarden.Sanitize
  ( Secret (SessionSecret),
    SanitizedText,
    getSanitizedText,
    sanitizeGetPasswordFailure,
    sanitizeListItemsFailure,
    sanitizeSyncFailure,
    sanitizeUnlockError,
    trustStaticText
  )

data Request
  = UnlockRequest Username Password
  | Status
  | ListItems
  | GetPasswordRequest LoginItemId
  | UnknownRequest
  deriving (Eq, Show)

instance Arbitrary Request where
  arbitrary =
    oneof
      [ UnlockRequest <$> arbitrary <*> arbitrary,
        pure Status,
        pure ListItems,
        GetPasswordRequest <$> arbitrary,
        pure UnknownRequest
      ]

data LatestRefreshStatus
  = LatestRefreshSucceeded
  | LatestRefreshFailed CacheFillFailure
  deriving (Eq, Show)

data CacheFillFailure
  = CacheFillUnavailable
  | CacheFillFailed (SanitizedText SessionSecret)
  deriving (Eq, Show)

instance Arbitrary CacheFillFailure where
  arbitrary =
    oneof
      [ pure CacheFillUnavailable,
        CacheFillFailed <$> arbitrary
      ]

instance Arbitrary LatestRefreshStatus where
  arbitrary =
    oneof
      [ pure LatestRefreshSucceeded,
        LatestRefreshFailed <$> arbitrary
      ]

data CacheEntry = CacheEntry
  { cacheEntryItems :: [ItemSummary],
    cacheEntryRefreshedAt :: UTCTime
  }
  deriving (Eq, Show)

instance Arbitrary CacheEntry where
  arbitrary =
    CacheEntry <$> arbitrary <*> arbitraryUtcTime

data ItemCacheState
  = CacheNotYetFilled
  | CacheFillError CacheFillFailure
  | CacheReady CacheEntry LatestRefreshStatus
  deriving (Eq, Show)

instance Arbitrary ItemCacheState where
  arbitrary =
    oneof
      [ pure CacheNotYetFilled,
        CacheFillError <$> arbitrary,
        CacheReady <$> arbitrary <*> arbitrary
      ]

data AgentState
  = Locked
  | Unlocked SessionKey ItemCacheState
  deriving (Eq, Show)

instance Arbitrary AgentState where
  arbitrary = do
    maybeSessionKey <- arbitrary
    case maybeSessionKey of
      Nothing -> pure Locked
      Just sessionKey -> Unlocked sessionKey <$> arbitrary

arbitraryUtcTime :: Gen UTCTime
arbitraryUtcTime =
  posixSecondsToUTCTime . fromInteger . abs <$> arbitrary

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    cmd <- obj .: "cmd"
    case (cmd :: Text) of
      "unlock" -> UnlockRequest <$> (Username <$> obj .: "email") <*> (Password <$> obj .: "password")
      "status" -> pure Status
      "list-items" -> pure ListItems
      "get-password" -> GetPasswordRequest . LoginItemId <$> obj .: "id"
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
  toJSON (GetPasswordRequest (LoginItemId passwordItemId)) =
    object
      [ "cmd" .= ("get-password" :: Text),
        "id" .= passwordItemId
      ]
  toJSON UnknownRequest =
    object
      [ "cmd" .= ("unknown" :: Text)
      ]

runAgent :: IO ()
runAgent = do
  runtimeDir <- requireRuntimeDir
  env <- initAgentEnv runtimeDir
  let paths = Runtime.deriveAgentPaths runtimeDir

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

buildInitialCacheState :: (Bitwarden m, MonadTime m) => SessionKey -> m ItemCacheState
buildInitialCacheState sessionKey =
  initialItemCacheState <$> refreshCacheEntry sessionKey

handleListItems :: MonadTime m => CacheEntry -> AgentState -> m (AgentState, Response, [Effect])
handleListItems cacheEntry agentState = do
  now <- currentTime
  pure (agentState, itemListResponse (cacheEntryItems cacheEntry) (cacheAgeSeconds now cacheEntry), [])

cacheAgeSeconds :: UTCTime -> CacheEntry -> CacheAgeSeconds
cacheAgeSeconds now cacheEntry =
  CacheAgeSeconds (floor (diffUTCTime now (cacheEntryRefreshedAt cacheEntry)))

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
    logInfo "starting item cache refresh loop"
    void $
      Concurrent.forkIO (refreshLoop refreshIntervalMicroseconds)
  where
    refreshLoop refreshIntervalMicroseconds = do
      Concurrent.threadDelay refreshIntervalMicroseconds
      logInfo "running item cache refresh"
      refreshResult <- refreshCacheEntry sessionKey
      shouldContinue <-
        modifyMVar agentStateVar $
          \agentState -> pure $ handleRefreshResult sessionKey refreshResult agentState
      logRefreshResult refreshResult shouldContinue
      when shouldContinue $
        refreshLoop refreshIntervalMicroseconds

refreshCacheEntry :: (Bitwarden m, MonadTime m) => SessionKey -> m (Either CacheFillFailure CacheEntry)
refreshCacheEntry sessionKey = runExceptT $ do
  ExceptT $ first syncErrorToCacheFailure <$> sync sessionKey
  items <- 
    ExceptT $ first (cacheFillFailureFromListItemsError sessionKey) <$> listItems sessionKey
  now <- currentTime
  pure $ CacheEntry items now


  where
    syncErrorToCacheFailure :: SyncError -> CacheFillFailure
    syncErrorToCacheFailure SyncUnavailable =
      syncCacheFillFailure sessionKey "bw sync was unavailable"
    syncErrorToCacheFailure (SyncFailed errMsg) =
      syncCacheFillFailure sessionKey ("bw sync failed due to " <> errMsg)

initialItemCacheState :: Either CacheFillFailure CacheEntry -> ItemCacheState
initialItemCacheState refreshResult =
  case refreshResult of
    Right cacheEntry ->
      CacheReady cacheEntry LatestRefreshSucceeded
    Left cacheFillFailure ->
      CacheFillError cacheFillFailure

cacheFillFailureFromListItemsError :: SessionKey -> ListItemsError -> CacheFillFailure
cacheFillFailureFromListItemsError sessionKey listItemsFailure =
  case listItemsFailure of
    ListItemsUnavailable ->
      CacheFillUnavailable
    ListItemsFailed err ->
      listItemsCacheFillFailure sessionKey err

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

updateItemCacheState :: ItemCacheState -> Either CacheFillFailure CacheEntry -> ItemCacheState
updateItemCacheState itemCacheState refreshResult =
  case (itemCacheState, refreshResult) of
    (_, Right cacheEntry) ->
      CacheReady cacheEntry LatestRefreshSucceeded
    (CacheReady cacheEntry _, Left cacheFillFailure) ->
      CacheReady cacheEntry (LatestRefreshFailed cacheFillFailure)
    (_, Left cacheFillFailure) ->
      CacheFillError cacheFillFailure

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

listItemsCacheFillFailure :: SessionKey -> Text -> CacheFillFailure
listItemsCacheFillFailure sessionKey err =
  CacheFillFailed (sanitizeListItemsFailure sessionKey err)

syncCacheFillFailure :: SessionKey -> Text -> CacheFillFailure
syncCacheFillFailure sessionKey err =
  CacheFillFailed (sanitizeSyncFailure sessionKey err)

logRefreshResult :: KatipContext m => Either CacheFillFailure CacheEntry -> Bool -> m ()
logRefreshResult refreshResult shouldContinue =
  case (refreshResult, shouldContinue) of
    (Right _, True) ->
      logInfo "item cache refresh succeeded"
    (Left CacheFillUnavailable, True) ->
      logInfo "item cache refresh failed: unavailable"
    (Left (CacheFillFailed err), True) ->
      logInfo ("item cache refresh failed: " <> getSanitizedText err)
    (_, False) ->
      logInfo "stopping item cache refresh loop"

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

cacheRefreshNamespace :: Namespace
cacheRefreshNamespace = "cache-refresh"
