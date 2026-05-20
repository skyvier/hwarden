{-# LANGUAGE OverloadedStrings #-}

module Integration (integrationTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Hwarden.Agent as Agent
import Hwarden.Bitwarden (determineBitwardenServerUrl)
import qualified Hwarden.Runtime as Runtime
import Hwarden.Socket (recvAll)
import qualified Data.Text as T
import Network.Socket
  ( Family (AF_UNIX),
    ShutdownCmd (ShutdownSend),
    SockAddr (SockAddrUnix),
    SocketType (Stream),
    close,
    connect,
    defaultProtocol,
    shutdown,
    socket
  )
import qualified Network.Socket.ByteString as NBS
import System.Directory
  ( Permissions (executable),
    createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
    getPermissions,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
    setPermissions
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process
  ( CreateProcess (cwd, env, std_err, std_out),
    ProcessHandle,
    StdStream (Inherit),
    createProcess,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

data CommandBehavior
  = CommandSucceeds BS8.ByteString
  | CommandFails BS8.ByteString

data BwBehavior = BwBehavior
  { logoutBehavior :: CommandBehavior,
    configServerBehavior :: CommandBehavior,
    unlockBehavior :: CommandBehavior,
    listItemsBehaviors :: [CommandBehavior],
    getPasswordBehavior :: CommandBehavior
  }

data AgentConfig = AgentConfig
  { agentBwBehavior :: BwBehavior,
    agentServerUrlOverride :: Maybe String,
    agentRefreshIntervalSecondsOverride :: Maybe Int
  }

data AgentResource = AgentResource
  { socketPath :: FilePath,
    processHandle :: ProcessHandle,
    tempRoot :: FilePath
  }

integrationTests :: TestTree
integrationTests =
  testGroup
    "integration"
    [ testCase "sending a status request via the socket to a fresh agent process results in a locked response" $ do
        agent <- setupAgent defaultAgentConfig
        response <- sendRequest (socketPath agent) Agent.Status
        cleanupAgent agent
        assertEqual
          "expected locked status response"
          (Agent.Success "locked")
          response

    -- setupAgent waits for the daemon to finish startup, and startup always
    -- runs `bw logout` before `bw config server`. That means even tests that
    -- only create and tear down the agent still exercise the fake `bw` script
    -- and verify the isolated BITWARDENCLI_APPDATA_DIR bootstrap path.
    , testCase "agent startup configures the default Bitwarden EU server in the isolated profile" $ do
        agent <- setupAgent defaultAgentConfig
        cleanupAgent agent
    , testCase "agent startup honors HWARDEN_SERVER_URL in the isolated profile" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentServerUrlOverride = Just "https://vault.example.test"
              }
        cleanupAgent agent
    , testCase "agent startup continues when bw logout fails before server configuration" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandFails "logout failed"
                    }
              }
        cleanupAgent agent
    -- This is a pragmatic race-based check, not a proof: we poll for process
    -- exit and socket creation, so the result still depends on scheduling.
    -- The goal is to catch regressions in the expected startup ordering without
    -- adding a more complex synchronization protocol to the fake `bw` script.
    , testCase "agent startup fails before creating the socket if bw config server fails" $ do
        agent <-
          spawnConfiguredAgent
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw {configServerBehavior = CommandFails "config failed"}
              }
        exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
        exitCode <- waitForProcess (processHandle agent)
        removeDirectoryRecursive (tempRoot agent)
        assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
        assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)
    , testCase "agent startup fails when bw config server fails after the logout attempt" $ do
        agent <-
          spawnConfiguredAgent
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandFails "config failed"
                    }
              }
        exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
        exitCode <- waitForProcess (processHandle agent)
        removeDirectoryRecursive (tempRoot agent)
        assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
        assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)

    , testCase "sending a list-items request via the socket to a fresh agent process results in a locked failure" $ do
        agent <- setupAgent defaultAgentConfig
        response <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual
          "expected locked list-items response"
          (Agent.Failure "locked")
          response
    , testCase "sending a get-password request via the socket to a fresh agent process results in a locked failure" $ do
        agent <- setupAgent defaultAgentConfig
        response <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
        cleanupAgent agent
        assertEqual
          "expected locked get-password response"
          (Agent.Failure "locked")
          response
    , testCase "sending status then successful unlock then status via the socket reports locked then unlocked" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior = defaultFailingBw {unlockBehavior = CommandSucceeds "session-key-123"}
              }
        initialStatus <- sendRequest (socketPath agent) Agent.Status
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        finalStatus <- sendRequest (socketPath agent) Agent.Status
        cleanupAgent agent
        assertEqual "expected initial locked status" (Agent.Success "locked") initialStatus
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual "expected unlocked status after successful unlock" (Agent.Success "unlocked") finalStatus
    , testCase "sending unlock then list-items via the socket returns login item summaries" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors = [CommandSucceeds listItemsPayload],
                      getPasswordBehavior = CommandFails "bw get password failed"
                    }
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual
          "expected listed login items"
          (Agent.ItemList listItemsSummary (Agent.CacheAgeSeconds 0))
          itemsResponse
    , testCase "sending unlock then waiting for the background refresh returns refreshed login item summaries" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload,
                          CommandSucceeds refreshedListItemsPayload
                        ],
                      getPasswordBehavior = CommandFails "bw get password failed"
                    },
                agentRefreshIntervalSecondsOverride = Just 1
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        itemsResponse <-
          waitForMatchingResponse
            (socketPath agent)
            Agent.ListItems
            (matchesExpectedItems refreshedListItemsSummary)
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertItemListMatches "expected refreshed login items" refreshedListItemsSummary itemsResponse
    , testCase "sending unlock with a failed initial cache fill eventually serves cached items after a background refresh" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors =
                        [ CommandFails "bw list items failed",
                          CommandSucceeds listItemsPayload
                        ],
                      getPasswordBehavior = CommandFails "bw get password failed"
                    },
                agentRefreshIntervalSecondsOverride = Just 1
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        initialItemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        recoveredItemsResponse <-
          waitForMatchingResponse
            (socketPath agent)
            Agent.ListItems
            (matchesExpectedItems listItemsSummary)
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual "expected cache-unavailable response before refresh succeeds" (Agent.Failure "item cache unavailable") initialItemsResponse
        assertItemListMatches "expected cached items after background refresh succeeds" listItemsSummary recoveredItemsResponse
    , testCase "sending unlock then waiting for a failed background refresh still serves stale cached items" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload,
                          CommandFails "bw list items failed"
                        ],
                      getPasswordBehavior = CommandFails "bw get password failed"
                    },
                agentRefreshIntervalSecondsOverride = Just 1
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        threadDelay 1200000
        itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        case itemsResponse of
          Agent.ItemList actualItems (Agent.CacheAgeSeconds ageSeconds) -> do
            assertEqual "expected stale cached items after refresh failure" listItemsSummary actualItems
            assertBool "expected stale cache age after refresh failure" (ageSeconds >= 1)
            assertBool "expected recent stale cache age after refresh failure" (ageSeconds <= 5)
          _ ->
            assertEqual
              "expected stale cached items after refresh failure"
              (Agent.ItemList listItemsSummary (Agent.CacheAgeSeconds 0))
              itemsResponse
    , testCase "sending unlock then get-password via the socket returns item id and password" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors = [CommandFails "bw list items failed"],
                      getPasswordBehavior = CommandSucceeds "super-secret"
                    }
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual
          "expected password result"
          (Agent.PasswordResult (Agent.LoginItemId "item-123") (Agent.PasswordValue "super-secret"))
          passwordResponse
    , testCase "sending unlock then get-password via the socket returns failure when bw get password fails" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors = [CommandFails "bw list items failed"],
                      getPasswordBehavior = CommandFails "item lookup failed"
                    }
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual
          "expected get-password failure response"
          (Agent.Failure "item lookup failed")
          passwordResponse
    , testCase "sending unlock then get-password via the socket returns failure when bw get password returns an empty password" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  BwBehavior
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandSucceeds "",
                      unlockBehavior = CommandSucceeds "session-key-123",
                      listItemsBehaviors = [CommandFails "bw list items failed"],
                      getPasswordBehavior = CommandSucceeds ""
                    }
              }
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        assertEqual
          "expected empty password failure response"
          (Agent.Failure "password was empty")
          passwordResponse
    , testCase "sending status then failed unlock then status via the socket reports locked then still locked" $ do
        agent <- setupAgent defaultAgentConfig
        initialStatus <- sendRequest (socketPath agent) Agent.Status
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
        finalStatus <- sendRequest (socketPath agent) Agent.Status
        cleanupAgent agent
        assertEqual "expected initial locked status" (Agent.Success "locked") initialStatus
        assertEqual "expected failed unlock response" (Agent.Failure "credentials were incorrect") unlockResponse
        assertEqual "expected locked status after failed unlock" (Agent.Success "locked") finalStatus
    , testCase "sending failed unlock then list-items via the socket still reports locked" $ do
        agent <- setupAgent defaultAgentConfig
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
        itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual "expected failed unlock response" (Agent.Failure "credentials were incorrect") unlockResponse
        assertEqual "expected locked list-items response after failed unlock" (Agent.Failure "locked") itemsResponse
    , testCase "sending invalid credentials via the socket results in failure message" $ do
        agent <- setupAgent defaultAgentConfig
        response <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
        cleanupAgent agent
        assertBool "expected failure response" (response /= Agent.Success "unlocked")
        assertEqual
          "expected invalid credentials error"
          (Agent.Failure "credentials were incorrect")
          response
    ]

setupAgent :: AgentConfig -> IO AgentResource
setupAgent agentConfig = do
  agent <- spawnConfiguredAgent agentConfig
  waitForSocketReady (socketPath agent)
  pure agent

cleanupAgent :: AgentResource -> IO ()
cleanupAgent agent = do
  terminateProcess (processHandle agent)
  _ <- waitForProcess (processHandle agent)
  removeDirectoryRecursive (tempRoot agent)

spawnAgent :: FilePath -> FilePath -> [(String, String)] -> IO ProcessHandle
spawnAgent hwardenAgent workDir agentEnv = do
  (_, _, _, handle) <-
    createProcess
      (proc hwardenAgent [])
        { cwd = Just workDir,
          env = Just agentEnv,
          std_out = Inherit,
          std_err = Inherit
        }
  pure handle

spawnConfiguredAgent :: AgentConfig -> IO AgentResource
spawnConfiguredAgent agentConfig = do
  tmpDir <- createTempDir "hwarden-agent-test"
  let paths = Runtime.deriveAgentPaths (tmpDir </> "runtime")
      fakeBinDir = tmpDir </> "bin"
      serverUrl = determineBitwardenServerUrl (agentServerUrlOverride agentConfig)
  createDirectoryIfMissing True (Runtime.runtimeDir paths)
  createDirectoryIfMissing True fakeBinDir
  writeFakeBw
    fakeBinDir
    (Runtime.bitwardenCliAppDataDir paths)
    (T.unpack serverUrl)
    (agentBwBehavior agentConfig)
  hwardenAgent <- requireExecutable "hwarden-agent"
  bwReal <- requireExecutable "bw"
  baseEnv <- getEnvironment
  let pathValue = fakeBinDir <> ":" <> takeDirectory bwReal
      applyServerUrlOverride =
        maybe id
          (setEnvVar "HWARDEN_SERVER_URL")
          (agentServerUrlOverride agentConfig)
      applyRefreshIntervalOverride =
        maybe id
          (setEnvVar "HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS" . show)
          (agentRefreshIntervalSecondsOverride agentConfig)
      agentBaseEnv =
        setEnvVar "PATH" pathValue
          (setEnvVar "XDG_RUNTIME_DIR" (Runtime.runtimeDir paths) baseEnv)
      agentEnv = applyRefreshIntervalOverride (applyServerUrlOverride agentBaseEnv)
  handle <- spawnAgent hwardenAgent tmpDir agentEnv
  pure
    AgentResource
      { socketPath = Runtime.socketPath paths,
        processHandle = handle,
        tempRoot = tmpDir
      }

waitForSocketReady :: FilePath -> IO ()
waitForSocketReady agentSocketPath = go (200 :: Int)
  where
    go 0 = fail ("socket was not created: " <> agentSocketPath)
    go retries = do
      exists <- doesFileExist agentSocketPath
      if exists
        then pure ()
        else threadDelay 50000 >> go (retries - 1)

waitForProcessExitBeforeSocketReady :: AgentResource -> IO Bool
waitForProcessExitBeforeSocketReady agent = go (200 :: Int)
  where
    go 0 = pure False
    go retries = do
      exists <- doesFileExist (socketPath agent)
      if exists
        then pure False
        else do
          exitCode <- getProcessExitCode (processHandle agent)
          case exitCode of
            Just _ -> pure True
            Nothing -> threadDelay 50000 >> go (retries - 1)

sendRequest :: FilePath -> Agent.Request -> IO Agent.Response
sendRequest agentSocketPath request =
  bracket open close $ \conn -> do
    NBS.sendAll conn (LBS.toStrict (Aeson.encode request))
    shutdown conn ShutdownSend
    responseBytes <- recvAll conn
    case Aeson.eitherDecode (LBS.fromStrict responseBytes) of
      Left decodeErr -> fail ("failed to decode response: " <> decodeErr)
      Right response -> pure response
  where
    open = do
      conn <- socket AF_UNIX Stream defaultProtocol
      connect conn (SockAddrUnix agentSocketPath)
      pure conn

writeFakeBw :: FilePath -> FilePath -> String -> BwBehavior -> IO ()
writeFakeBw fakeBinDir expectedAppDataDir expectedServerUrl bwBehavior = do
  let fakeBw = fakeBinDir </> "bw"
  BS8.writeFile
    fakeBw
    (scriptFor expectedAppDataDir expectedServerUrl bwBehavior)
  permissions <- getPermissions fakeBw
  setPermissions fakeBw permissions {executable = True}

scriptFor :: FilePath -> String -> BwBehavior -> BS8.ByteString
scriptFor expectedAppDataDir expectedServerUrl bwBehavior =
  BS8.unlines
    [ "#!/bin/sh",
      "if [ -z \"$BITWARDENCLI_APPDATA_DIR\" ]; then",
      "  printf '%s\\n' 'BITWARDENCLI_APPDATA_DIR was not set' 1>&2",
      "  exit 1",
      "fi",
      "if [ \"$BITWARDENCLI_APPDATA_DIR\" != \"" <> BS8.pack expectedAppDataDir <> "\" ]; then",
      "  printf '%s\\n' 'BITWARDENCLI_APPDATA_DIR did not match expected path' 1>&2",
      "  exit 1",
      "fi",
      "case \"$1\" in",
      "  logout)",
      emitLogoutBehavior "    " (logoutBehavior bwBehavior),
      "    ;;",
      "  config)",
      "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\" ]; then",
      "      printf '%s\\n' 'logout was not attempted before config' 1>&2",
      "      exit 1",
      "    fi",
      "    if [ \"$2\" = \"server\" ] && [ \"$3\" = \"" <> BS8.pack expectedServerUrl <> "\" ]; then",
      emitConfigBehavior "      " (configServerBehavior bwBehavior),
      "    else",
      "      printf '%s\\n' 'unexpected bw config server invocation' 1>&2",
      "      exit 1",
      "    fi",
      "    ;;",
      "  login)",
      "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then",
      "      printf '%s\\n' 'server was not configured before login' 1>&2",
      "      exit 1",
      "    fi",
      emitBehavior "    " (unlockBehavior bwBehavior),
      "    ;;",
      "  list)",
      "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then",
      "      printf '%s\\n' 'server was not configured before list' 1>&2",
      "      exit 1",
      "    fi",
      "    if [ \"$2\" = \"items\" ]; then",
      "      LIST_ITEMS_COUNT_FILE=\"$BITWARDENCLI_APPDATA_DIR/list-items-count\"",
      "      list_items_count=0",
      "      if [ -f \"$LIST_ITEMS_COUNT_FILE\" ]; then IFS= read -r list_items_count < \"$LIST_ITEMS_COUNT_FILE\"; fi",
      "      printf '%s' $((list_items_count + 1)) > \"$LIST_ITEMS_COUNT_FILE\"",
      emitIndexedBehavior "      " (listItemsBehaviors bwBehavior),
      "    else",
      "      printf '%s\\n' 'unsupported list command' 1>&2",
      "      exit 1",
      "    fi",
      "    ;;",
      "  get)",
      "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then",
      "      printf '%s\\n' 'server was not configured before get' 1>&2",
      "      exit 1",
      "    fi",
      "    if [ \"$2\" = \"password\" ]; then",
      emitBehavior "      " (getPasswordBehavior bwBehavior),
      "    else",
      "      printf '%s\\n' 'unsupported get command' 1>&2",
      "      exit 1",
      "    fi",
      "    ;;",
      "  *)",
      "    printf '%s\\n' 'unsupported bw command' 1>&2",
      "    exit 1",
      "    ;;",
      "esac"
    ]

emitBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds output ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\"; done <<'EOF'",
          output,
          "EOF",
          indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'",
          errMessage,
          "EOF",
          indent <> "exit 1"
        ]

emitIndexedBehavior :: BS8.ByteString -> [CommandBehavior] -> BS8.ByteString
emitIndexedBehavior indent commandBehaviors =
  BS8.unlines $
    [ indent <> "case \"$list_items_count\" in"
    ]
      <> zipWith (emitIndexedCase indent) [0 :: Int ..] commandBehaviors
      <> [ indent <> "  *)",
           indent <> "    printf '%s\\n' 'unsupported list command count' 1>&2",
           indent <> "    exit 1",
           indent <> "    ;;",
           indent <> "esac"
         ]

emitIndexedCase :: BS8.ByteString -> Int -> CommandBehavior -> BS8.ByteString
emitIndexedCase indent index commandBehavior =
  BS8.unlines
    [ indent <> "  " <> BS8.pack (show index) <> ")",
      emitBehavior (indent <> "    ") commandBehavior,
      indent <> "    ;;"
    ]

emitLogoutBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitLogoutBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\"",
          indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\"",
          indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'",
          errMessage,
          "EOF",
          indent <> "exit 1"
        ]

emitConfigBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitConfigBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/configured\"",
          indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'",
          errMessage,
          "EOF",
          indent <> "exit 1"
        ]

defaultFailingBw :: BwBehavior
defaultFailingBw =
  BwBehavior
    { logoutBehavior = CommandSucceeds "",
      configServerBehavior = CommandSucceeds "",
      unlockBehavior = CommandFails "credentials were incorrect",
      listItemsBehaviors = [CommandFails "bw list items failed"],
      getPasswordBehavior = CommandFails "bw get password failed"
    }

defaultAgentConfig :: AgentConfig
defaultAgentConfig =
  AgentConfig
    { agentBwBehavior = defaultFailingBw,
      agentServerUrlOverride = Nothing,
      agentRefreshIntervalSecondsOverride = Nothing
    }

listItemsPayload :: BS8.ByteString
listItemsPayload =
  BS8.unlines
    [ "[",
      "  {",
      "    \"id\": \"1\",",
      "    \"name\": \"Battle.net\",",
      "    \"login\": {",
      "      \"username\": \"joonas_laukka@hotmail.com\"",
      "    }",
      "  },",
      "  {",
      "    \"id\": \"ignored\",",
      "    \"name\": \"Secure note\",",
      "    \"notes\": \"not a login\"",
      "  },",
      "  {",
      "    \"id\": \"2\",",
      "    \"name\": \"GitHub\",",
      "    \"login\": {",
      "      \"username\": \"skyvier\"",
      "    }",
      "  }",
      "]"
    ]

-- Keep this summary next to listItemsPayload: the integration assertion is
-- intentionally derived from the same mocked CLI payload.
listItemsSummary :: [Agent.ItemSummary]
listItemsSummary =
  [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com",
    Agent.ItemSummary "2" "GitHub" "skyvier"
  ]

refreshedListItemsPayload :: BS8.ByteString
refreshedListItemsPayload =
  BS8.unlines
    [ "[",
      "  {",
      "    \"id\": \"3\",",
      "    \"name\": \"Fastmail\",",
      "    \"login\": {",
      "      \"username\": \"skyvier@example.com\"",
      "    }",
      "  }",
      "]"
    ]

refreshedListItemsSummary :: [Agent.ItemSummary]
refreshedListItemsSummary =
  [ Agent.ItemSummary "3" "Fastmail" "skyvier@example.com"
  ]

assertItemListMatches :: String -> [Agent.ItemSummary] -> Agent.Response -> IO ()
assertItemListMatches message expectedItems response =
  case response of
    Agent.ItemList actualItems _ ->
      assertEqual message expectedItems actualItems
    -- Keep the expected ItemList shape in the failure output when the
    -- response constructor is wrong.
    _ ->
      assertEqual message (Agent.ItemList expectedItems (Agent.CacheAgeSeconds 0)) response

waitForMatchingResponse :: FilePath -> Agent.Request -> (Agent.Response -> Bool) -> IO Agent.Response
waitForMatchingResponse agentSocketPath request matchesResponse =
  go (60 :: Int)
  where
    go 0 = sendRequest agentSocketPath request
    go retriesRemaining = do
      response <- sendRequest agentSocketPath request
      if matchesResponse response
        then pure response
        else threadDelay 50000 >> go (retriesRemaining - 1)

matchesExpectedItems :: [Agent.ItemSummary] -> Agent.Response -> Bool
matchesExpectedItems expectedItems response =
  case response of
    Agent.ItemList actualItems _ -> actualItems == expectedItems
    _ -> False

requireExecutable :: String -> IO FilePath
requireExecutable name = do
  path <- findExecutable name
  case path of
    Just executablePath -> pure executablePath
    Nothing -> fail ("missing executable in PATH: " <> name)

setEnvVar :: String -> String -> [(String, String)] -> [(String, String)]
setEnvVar key value envVars = (key, value) : filter ((/= key) . fst) envVars

createTempDir :: String -> IO FilePath
createTempDir prefix = do
  tempBase <- getTemporaryDirectory
  (tempPath, tempHandle) <- openTempFile tempBase prefix
  hClose tempHandle
  removeFile tempPath
  createDirectoryIfMissing True tempPath
  pure tempPath
