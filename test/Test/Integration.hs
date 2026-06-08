{-# LANGUAGE OverloadedStrings #-}

module Test.Integration (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Hwarden.Agent as Agent
import Hwarden.Bitwarden (determineBitwardenServerUrl)
import Hwarden.Runtime (AgentPaths)
import qualified Hwarden.Runtime as Runtime
import Hwarden.Socket (recvAll)
import Network.Socket (
  Family (AF_UNIX),
  ShutdownCmd (ShutdownSend),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  close,
  connect,
  defaultProtocol,
  shutdown,
  socket,
 )
import qualified Network.Socket.ByteString as NBS
import System.Directory (
  Permissions (executable),
  createDirectoryIfMissing,
  doesFileExist,
  getPermissions,
  setPermissions,
 )
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Process (
  CreateProcess (cwd, env, std_err, std_out),
  ProcessHandle,
  StdStream (..),
  createProcess,
  getPid,
  getProcessExitCode,
  proc,
  readCreateProcessWithExitCode,
  readProcess,
  terminateProcess,
  waitForProcess,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

data CommandBehavior
  = CommandSucceeds BS8.ByteString
  | CommandFails BS8.ByteString
  | CommandArbitrary (BS8.ByteString -> BS8.ByteString)

data LoginBehavior
  = LoginCommandBehavior CommandBehavior
  | LoginRequiresCode

data BwBehavior = BwBehavior
  { logoutBehavior :: CommandBehavior
  , configServerBehavior :: CommandBehavior
  , unlockBehavior :: LoginBehavior
  , listItemsBehaviors :: [CommandBehavior]
  , syncBehavior :: [CommandBehavior]
  , getPasswordBehavior :: CommandBehavior
  , lockBehavior :: CommandBehavior
  }

data BwPathMode
  = UseFakeBwPath
  | OmitBwPath

data AgentConfig = AgentConfig
  { agentBwBehavior :: BwBehavior
  , agentBwPathMode :: BwPathMode
  , agentPathOverride :: Maybe String
  , agentServerUrl :: Maybe String
  , agentRefreshIntervalSeconds :: Maybe Int
  }

data AgentResource = AgentResource
  { socketPath :: FilePath
  , processHandle :: ProcessHandle
  , runtimePaths :: AgentPaths
  , tempRoot :: FilePath
  }

tests :: TestTree
tests =
  testGroup
    "integration"
    [ testCase "sending a status request via the socket to a fresh agent process results in a locked response" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { configServerBehavior = CommandSucceeds "configuration succeeds"
                    -- agent won't start if it fails to configure itself
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            response <- sendRequest (socketPath agent) Agent.Status
            assertEqual
              "expected locked status response"
              (Agent.successResponse "locked")
              response
    , -- setupAgent waits for the daemon to finish startup, and startup always
      -- runs `bw logout` before `bw config server`. That means even tests that
      -- only create and tear down the agent still exercise the fake `bw` script.

      -- the fake bw script (see 'scriptFor') tests whether or not "bw config server"
      -- is called with the expected default value for it... I know it's complex
      testCase "agent startup configures the default Bitwarden EU server in the isolated profile" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { configServerBehavior = CommandSucceeds "configuration succeeds"
                    -- agent won't start if it fails to configure itself
                    }
              }
         in
          withReadyAgent agentConfig (\_ -> return ())
    , -- the fake bw script (see 'scriptFor') tests whether or not "bw config server"
      -- is called with the expected value for it, set via the HWARDEN_SERVER_URL
      -- env var, the logic is hidden in withConfiguredAgent
      testCase "agent startup honors HWARDEN_SERVER_URL in the isolated profile" $ do
        let
          agentConfig =
            defaultAgentConfig
              { agentServerUrl = Just "https://vault.example.test"
              , agentBwBehavior =
                  defaultFailingBw
                    { configServerBehavior = CommandSucceeds "configuration succeeds"
                    -- agent won't start if it fails to configure itself
                    }
              }
         in
          withReadyAgent agentConfig (\_ -> return ())
    , -- if logout failed, the server still becomes ready (socket is created)
      testCase "agent startup continues when bw logout fails before server configuration" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandFails ""
                    , configServerBehavior = CommandSucceeds ""
                    }
              }
         in
          withReadyAgent agentConfig $ \_ -> return ()
    , -- This is a pragmatic race-based check, not a proof: we poll for process
      -- exit and socket creation, so the result still depends on scheduling.
      -- The goal is to catch regressions in the expected startup ordering without
      -- adding a more complex synchronization protocol to the fake `bw` script.
      testCase "agent startup fails before creating the socket if bw config server fails" $ do
        withConfiguredAgent defaultAgentConfig $ \agent -> do
          exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
          exitCode <- waitForProcess (processHandle agent)
          assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
          assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)
    , testCase "agent startup fails when bw config server fails after the logout attempt" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandFails "config failed"
                    }
              }
         in
          withConfiguredAgent agentConfig $ \agent -> do
            exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
            exitCode <- waitForProcess (processHandle agent)
            assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
            assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)
    , testCase "agent startup fails before creating the socket when HWARDEN_BW_PATH is missing" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwPathMode = OmitBwPath
              }
         in
          withConfiguredAgent agentConfig $ \agent -> do
            exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
            exitCode <- waitForProcess (processHandle agent)
            assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
            assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)
    , testCase "agent env can set an empty PATH while still injecting HWARDEN_BW_PATH" $ do
        withEnvVarOverride "PATH" (Just "/bin:/usr/bin") $ do
          withAgentEnv
            defaultAgentConfig{agentPathOverride = Just ""}
            "/tmp/runtime"
            "/tmp/config"
            "/tmp/fake-bw"
            $ \_ -> do
              mHwardenBwPath <- lookupEnv "HWARDEN_BW_PATH"
              mHwardenBwPath @?= Just "/tmp/fake-bw"
              mPath <- lookupEnv "PATH"
              mPath @?= Nothing
    , testCase "agent executable lookup honors HWARDEN_AGENT_TEST_EXE when PATH is empty" $ do
        withSystemTempDirectory "hwarden-agent-exe" $ \tempDir -> do
          let fakeAgentPath = tempDir </> "hwarden-agent"
          BS8.writeFile fakeAgentPath "#!/bin/sh\nexit 0\n"
          permissions <- getPermissions fakeAgentPath
          setPermissions fakeAgentPath permissions{executable = True}
          executablePath <-
            withEnvVarOverride "PATH" (Just "") $
              withEnvVarOverride "HWARDEN_AGENT_TEST_EXE" (Just fakeAgentPath) $
                requireAgentExecutable
          executablePath @?= fakeAgentPath
    , testCase "agent executable lookup falls back to cabal list-bin instead of PATH" $ do
        executablePath <-
          requireAgentExecutableWith
            (pure Nothing)
            (pure "/tmp/from-cabal/hwarden-agent")
        executablePath @?= "/tmp/from-cabal/hwarden-agent"
    , testCase "hwarden-agent version prints HWARDEN_VERSION and exits" $ do
        hwardenAgent <- requireAgentExecutable
        let versionEnv = [("HWARDEN_VERSION", "test-hash-123")]
        (exitCode, stdoutText, stderrText) <-
          readCreateProcessWithExitCode
            ((proc hwardenAgent ["version"]){env = Just versionEnv})
            ""
        exitCode @?= ExitSuccess
        stdoutText @?= "test-hash-123\n"
        stderrText @?= ""
    , testCase "hwarden-agent version prints unknown when HWARDEN_VERSION is unset" $ do
        hwardenAgent <- requireAgentExecutable
        (exitCode, stdoutText, stderrText) <-
          readCreateProcessWithExitCode
            ((proc hwardenAgent ["version"]){env = Just []})
            ""
        exitCode @?= ExitSuccess
        stdoutText @?= "unknown\n"
        stderrText @?= ""
    , testCase "sending a list-items request via the socket to a fresh agent process results in a locked failure" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior = defaultStartingBw
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            response <- sendRequest (socketPath agent) Agent.ListItems
            assertEqual
              "expected locked list-items response"
              (Agent.failureResponse "locked")
              response
    , testCase "sending a get-password request via the socket to a fresh agent process results in a locked failure" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior = defaultStartingBw
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            response <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            assertEqual
              "expected locked get-password response"
              (Agent.failureResponse "locked")
              response
    , testCase "sending status then successful unlock then status via the socket reports locked then unlocked" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            initialStatus <- sendRequest (socketPath agent) Agent.Status
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            finalStatus <- sendRequest (socketPath agent) Agent.Status
            assertEqual "expected initial locked status" (Agent.successResponse "locked") initialStatus
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual "expected unlocked status after successful unlock" (Agent.successResponse "unlocked") finalStatus
    , testCase "sending unlock then list-items via the socket returns login item summaries" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors = [CommandSucceeds listItemsPayload]
                    , syncBehavior = [CommandSucceeds ""]
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual
              "expected listed login items"
              (Agent.itemListResponse listItemsSummary (Agent.CacheAgeSeconds 0) Agent.CacheRefreshSucceeded)
              itemsResponse
    , testCase "sending unlock then waiting for the background refresh returns refreshed login item summaries" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { logoutBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload
                        , CommandSucceeds refreshedListItemsPayload
                        ]
                    , syncBehavior =
                        [ CommandSucceeds ""
                        , CommandSucceeds ""
                        ]
                    }
              , agentRefreshIntervalSeconds = Just 1
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            itemsResponse <-
              waitForMatchingResponse
                (socketPath agent)
                Agent.ListItems
                (matchesExpectedItems refreshedListItemsSummary)
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertItemListMatches "expected refreshed login items" refreshedListItemsSummary itemsResponse
    , testCase "sending unlock with a failed initial cache fill eventually serves cached items after a background refresh" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors =
                        [ CommandFails "bw list items failed"
                        , CommandSucceeds listItemsPayload
                        ]
                    , syncBehavior =
                        [ CommandSucceeds ""
                        , CommandSucceeds ""
                        ]
                    }
              , agentRefreshIntervalSeconds = Just 1
              }
         in
          withReadyAgent agentConfig $ \agent -> do
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
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual "expected cache-unavailable response before refresh succeeds" (Agent.failureResponse "item cache unavailable") initialItemsResponse
            assertItemListMatches "expected cached items after background refresh succeeds" listItemsSummary recoveredItemsResponse
    , testCase "sending unlock then waiting for a failed background refresh still serves stale cached items" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload
                        , CommandFails "bw list items failed"
                        ]
                    , syncBehavior = [CommandSucceeds ""]
                    }
              , agentRefreshIntervalSeconds = Just 1
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            threadDelay 1200000
            itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            case Agent.responseItems itemsResponse of
              Just (actualItems, Agent.CacheAgeSeconds ageSeconds, cacheRefreshStatus) -> do
                assertEqual "expected stale cached items after refresh failure" listItemsSummary actualItems
                assertBool "expected stale cache age after refresh failure" (ageSeconds >= 1)
                assertBool "expected recent stale cache age after refresh failure" (ageSeconds <= 5)
                assertEqual "expected failed cache refresh status" Agent.CacheRefreshFailed cacheRefreshStatus
              Nothing ->
                assertEqual
                  "expected stale cached items after refresh failure"
                  (Agent.itemListResponse listItemsSummary (Agent.CacheAgeSeconds 0) Agent.CacheRefreshFailed)
                  itemsResponse
    , testCase "bw sync is called during cache refresh" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { logoutBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload
                        , CommandSucceeds refreshedListItemsPayload
                        ]
                    , syncBehavior =
                        [ CommandSucceeds ""
                        , CommandArbitrary mksyncScript
                        ]
                    }
              , agentRefreshIntervalSeconds = Just 1
              }

          mksyncScript indent =
            BS8.unlines
              [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/synced\""
              , indent <> "exit 0"
              ]
         in
          withReadyAgent agentConfig $ \agent -> do
            let
              appDataDir = Runtime.bitwardenCliAppDataDir $ runtimePaths agent
              syncFile = appDataDir </> "synced"

            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))

            itemsResponse <-
              waitForMatchingResponse
                (socketPath agent)
                Agent.ListItems
                (matchesExpectedItems refreshedListItemsSummary)

            syncWasCalled <- doesFileExist syncFile

            assertBool "bw sync was not called" syncWasCalled
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertItemListMatches "expected refreshed login items" refreshedListItemsSummary itemsResponse
    , testCase "background cache refresh fails if \"bw sync\" fails" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { logoutBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , listItemsBehaviors =
                        [ CommandSucceeds listItemsPayload
                        , CommandSucceeds refreshedListItemsPayload
                        ]
                    , syncBehavior =
                        [ CommandSucceeds ""
                        , CommandFails ""
                        ]
                    }
              , agentRefreshIntervalSeconds = Just 1
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            let
              appDataDir = Runtime.bitwardenCliAppDataDir $ runtimePaths agent
              syncCountFile = appDataDir </> "sync-items-count"
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            syncAttempted <- waitForFileContent syncCountFile "2"
            itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertBool "expected background sync attempt" syncAttempted
            assertItemListMatches "expected unrefreshed login items" listItemsSummary itemsResponse
    , testCase "sending unlock then get-password via the socket returns item id and password" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , getPasswordBehavior = CommandSucceeds "super-secret"
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual
              "expected password result"
              (Agent.passwordResultResponse (Agent.LoginItemId "item-123") (Agent.PasswordValue "super-secret"))
              passwordResponse
    , testCase "sending unlock then get-password via the socket returns failure when bw get password fails" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , getPasswordBehavior = CommandFails "item lookup failed"
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual
              "expected get-password failure response"
              (Agent.failureResponse "item lookup failed")
              passwordResponse
    , testCase "sending unlock then get-password via the socket returns failure when bw get password returns an empty password" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , getPasswordBehavior = CommandSucceeds ""
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            passwordResponse <- sendRequest (socketPath agent) (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse
            assertEqual
              "expected empty password failure response"
              (Agent.failureResponse "password was empty")
              passwordResponse
    , testCase "SIGTERM triggers shutdown cleanup for a live session" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds ""
                    , configServerBehavior = CommandSucceeds ""
                    , unlockBehavior = LoginCommandBehavior (CommandSucceeds "session-key-123")
                    , lockBehavior = CommandSucceeds ""
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            let appDataDir = Runtime.bitwardenCliAppDataDir $ runtimePaths agent
                lockFile = appDataDir </> "lock-attempted"
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            assertEqual "expected successful unlock response" (Agent.successResponse "unlocked") unlockResponse

            maybePid <- getPid (processHandle agent)
            case maybePid of
              Nothing -> fail "agent process had no process id"
              Just pid -> signalProcess sigTERM pid
            exitCode <- waitForProcess (processHandle agent)
            lockAttempted <- doesFileExist lockFile

            assertEqual "expected graceful SIGTERM exit" ExitSuccess exitCode
            assertBool "expected bw lock during SIGTERM shutdown" lockAttempted
    , testCase "sending unlock fails when bw requires a two-factor code" $
        let
          agentConfig =
            defaultAgentConfig
              { agentBwBehavior =
                  defaultStartingBw
                    { unlockBehavior = LoginRequiresCode
                    }
              }
         in
          withReadyAgent agentConfig $ \agent -> do
            unlockResponse <-
              sendRequest
                (socketPath agent)
                (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
            finalStatus <- sendRequest (socketPath agent) Agent.Status
            assertEqual
              "expected helpful OTP failure response"
              (Agent.failureResponse "two-factor code required; run scripts/hwarden-first-login")
              unlockResponse
            assertEqual "expected locked status after missing code failure" (Agent.successResponse "locked") finalStatus
    , testCase "sending status then failed unlock then status via the socket reports locked then still locked" $
        withReadyAgent defaultStartingAgentConfig $ \agent -> do
          initialStatus <- sendRequest (socketPath agent) Agent.Status
          unlockResponse <-
            sendRequest
              (socketPath agent)
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
          finalStatus <- sendRequest (socketPath agent) Agent.Status
          assertEqual "expected initial locked status" (Agent.successResponse "locked") initialStatus
          assertEqual "expected failed unlock response" (Agent.failureResponse "credentials were incorrect") unlockResponse
          assertEqual "expected locked status after failed unlock" (Agent.successResponse "locked") finalStatus
    , testCase "sending failed unlock then list-items via the socket still reports locked" $
        withReadyAgent defaultStartingAgentConfig $ \agent -> do
          unlockResponse <-
            sendRequest
              (socketPath agent)
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
          itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
          assertEqual "expected failed unlock response" (Agent.failureResponse "credentials were incorrect") unlockResponse
          assertEqual "expected locked list-items response after failed unlock" (Agent.failureResponse "locked") itemsResponse
    , testCase "sending invalid credentials via the socket results in failure message" $
        withReadyAgent defaultStartingAgentConfig $ \agent -> do
          response <-
            sendRequest
              (socketPath agent)
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
          assertBool "expected failure response" (response /= Agent.successResponse "unlocked")
          assertEqual
            "expected invalid credentials error"
            (Agent.failureResponse "credentials were incorrect")
            response
    ]

withReadyAgent :: AgentConfig -> (AgentResource -> IO a) -> IO a
withReadyAgent agentConfig cont = do
  withConfiguredAgent agentConfig $ \agent -> do
    waitForSocketReady (socketPath agent)
    cont agent

withAgent :: FilePath -> FilePath -> [(String, String)] -> (ProcessHandle -> IO a) -> IO a
withAgent hwardenAgent workDir agentEnv cont =
  withFile "/dev/null" WriteMode $ \devNull -> do
    bracket
      (acquireProcessHandle devNull)
      releaseProcessHandle
      cont
 where
  acquireProcessHandle devNull = do
    (_, _, _, handle) <-
      createProcess
        (proc hwardenAgent [])
          { cwd = Just workDir
          , env = Just agentEnv
          , std_out = UseHandle devNull
          , std_err = UseHandle devNull
          }
    return handle

  releaseProcessHandle handle = do
    terminateProcess handle
    void $ waitForProcess handle

withConfiguredAgent :: AgentConfig -> (AgentResource -> IO a) -> IO a
withConfiguredAgent agentConfig cont =
  withTempDirectory "/tmp" "hwarden-agent-test" $ \tmpDir -> do
    let configDir = tmpDir </> "config"
        persistentAppDataDir = configDir </> "hwarden" </> "bitwarden-cli"
    paths <-
      either fail pure $
        Runtime.deriveAgentPaths
          (tmpDir </> "runtime")
          persistentAppDataDir
    let fakeBinDir = tmpDir </> "bin"
        fakeBwPath = fakeBinDir </> "bw"
        serverUrl = determineBitwardenServerUrl (agentServerUrl agentConfig)
    createDirectoryIfMissing True (Runtime.runtimeDir paths)
    createDirectoryIfMissing True fakeBinDir
    writeFakeBw
      fakeBwPath
      (Runtime.bitwardenCliAppDataDir paths)
      (T.unpack serverUrl)
      (agentBwBehavior agentConfig)
    hwardenAgent <- requireAgentExecutable
    withAgentEnv
      agentConfig
      (Runtime.runtimeDir paths)
      configDir
      fakeBwPath
      $ \agentEnv -> do
        withAgent hwardenAgent tmpDir agentEnv $ \handle ->
          cont $
            AgentResource
              { socketPath = Runtime.socketPath paths
              , processHandle = handle
              , runtimePaths = paths
              , tempRoot = tmpDir
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

writeFakeBw ::
  FilePath ->
  FilePath ->
  String ->
  BwBehavior ->
  IO ()
writeFakeBw fakeBw expectedAppDataDir expectedServerUrl bwBehavior = do
  let
    fakeBwScriptContent =
      scriptFor fakeBw expectedAppDataDir expectedServerUrl bwBehavior
  BS8.writeFile
    fakeBw
    fakeBwScriptContent
  permissions <- getPermissions fakeBw
  setPermissions fakeBw permissions{executable = True}

scriptFor :: FilePath -> FilePath -> String -> BwBehavior -> BS8.ByteString
scriptFor expectedExecutablePath expectedAppDataDir expectedServerUrl bwBehavior =
  BS8.unlines
    [ "#!/bin/sh"
    , "if [ \"$0\" != \"" <> BS8.pack expectedExecutablePath <> "\" ]; then"
    , "  printf '%s\\n' 'bw executable path did not match HWARDEN_BW_PATH' 1>&2"
    , "  exit 1"
    , "fi"
    , "if [ -z \"$BITWARDENCLI_APPDATA_DIR\" ]; then"
    , "  printf '%s\\n' 'BITWARDENCLI_APPDATA_DIR was not set' 1>&2"
    , "  exit 1"
    , "fi"
    , "if [ \"$BITWARDENCLI_APPDATA_DIR\" != \"" <> BS8.pack expectedAppDataDir <> "\" ]; then"
    , "  printf '%s\\n' 'BITWARDENCLI_APPDATA_DIR did not match expected path' 1>&2"
    , "  exit 1"
    , "fi"
    , "if [ \"$1\" = \"login\" ] && [ \"$2\" != \"--nointeraction\" ]; then"
    , "  printf '%s\\n' 'bw login was not run with --nointeraction' 1>&2"
    , "  exit 1"
    , "fi"
    , "case \"$1\" in"
    , "  logout)"
    , emitLogoutBehavior "    " (logoutBehavior bwBehavior)
    , "    ;;"
    , "  config)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\" ]; then"
    , "      printf '%s\\n' 'logout was not attempted before config' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$2\" = \"server\" ] && [ \"$3\" = \"" <> BS8.pack expectedServerUrl <> "\" ]; then"
    , emitConfigBehavior "      " (configServerBehavior bwBehavior)
    , "    else"
    , "      printf '%s\\n' 'unexpected bw config server invocation' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    ;;"
    , "  login)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then"
    , "      printf '%s\\n' 'server was not configured before login' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$2\" != \"--nointeraction\" ]; then"
    , "      printf '%s\\n' 'bw login was not run with --nointeraction' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$4\" != \"--passwordenv\" ]; then"
    , "      printf '%s\\n' 'bw login did not use --passwordenv' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    password_env_name=$5"
    , "    if [ -z \"$password_env_name\" ]; then"
    , "      printf '%s\\n' 'bw login password env name was empty' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    eval \"password_value=\\${$password_env_name-}\""
    , "    if [ -z \"$password_value\" ]; then"
    , "      printf '%s\\n' 'bw login password env value was not set' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$6\" != \"--raw\" ]; then"
    , "      printf '%s\\n' 'bw login did not request raw output' 1>&2"
    , "      exit 1"
    , "    fi"
    , emitLoginBehavior "    " (unlockBehavior bwBehavior)
    , "    ;;"
    , "  list)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then"
    , "      printf '%s\\n' 'server was not configured before list' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$2\" = \"items\" ]; then"
    , "      LIST_ITEMS_COUNT_FILE=\"$BITWARDENCLI_APPDATA_DIR/list-items-count\""
    , "      list_items_count=0"
    , "      if [ -f \"$LIST_ITEMS_COUNT_FILE\" ]; then IFS= read -r list_items_count < \"$LIST_ITEMS_COUNT_FILE\"; fi"
    , "      printf '%s' $((list_items_count + 1)) > \"$LIST_ITEMS_COUNT_FILE\""
    , emitIndexedBehavior "list_items_count" "      " (listItemsBehaviors bwBehavior)
    , "    else"
    , "      printf '%s\\n' 'unsupported list command' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    ;;"
    , "  get)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then"
    , "      printf '%s\\n' 'server was not configured before get' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ \"$2\" = \"password\" ]; then"
    , emitBehavior "      " (getPasswordBehavior bwBehavior)
    , "    else"
    , "      printf '%s\\n' 'unsupported get command' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    ;;"
    , "  sync)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/configured\" ]; then"
    , "      printf '%s\\n' 'server was not configured before sync' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/login-success\" ]; then"
    , "      printf '%s\\n' 'login was not successful before sync' 1>&2"
    , "      exit 1"
    , "    fi"
    , "    SYNC_ITEMS_COUNT_FILE=\"$BITWARDENCLI_APPDATA_DIR/sync-items-count\""
    , "    sync_items_count=0"
    , "    if [ -f \"$SYNC_ITEMS_COUNT_FILE\" ]; then IFS= read -r sync_items_count < \"$SYNC_ITEMS_COUNT_FILE\"; fi"
    , "    printf '%s' $((sync_items_count + 1)) > \"$SYNC_ITEMS_COUNT_FILE\""
    , emitIndexedBehavior "sync_items_count" "    " (syncBehavior bwBehavior)
    , "    ;;"
    , "  lock)"
    , "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/login-success\" ]; then"
    , "      printf '%s\\n' 'login was not successful before lock' 1>&2"
    , "      exit 1"
    , "    fi"
    , emitLockBehavior "    " (lockBehavior bwBehavior)
    , "    ;;"
    , "  *)"
    , "    printf '%s\\n' 'unsupported bw command' 1>&2"
    , "    exit 1"
    , "    ;;"
    , "esac"
    ]

emitBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds output ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\"; done <<'EOF'"
        , output
        , "EOF"
        , indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'"
        , errMessage
        , "EOF"
        , indent <> "exit 1"
        ]
    CommandArbitrary mkCommand -> mkCommand indent

emitIndexedBehavior ::
  BS8.ByteString ->
  BS8.ByteString ->
  [CommandBehavior] ->
  BS8.ByteString
emitIndexedBehavior variableName indent commandBehaviors =
  BS8.unlines $
    [ indent <> "case \"$" <> variableName <> "\" in"
    ]
      <> zipWith (emitIndexedCase indent) [0 :: Int ..] commandBehaviors
      <> [ indent <> "  *)"
         , indent <> "    printf '%s\\n' 'unsupported command count' 1>&2"
         , indent <> "    exit 1"
         , indent <> "    ;;"
         , indent <> "esac"
         ]

emitIndexedCase :: BS8.ByteString -> Int -> CommandBehavior -> BS8.ByteString
emitIndexedCase indent index commandBehavior =
  BS8.unlines
    [ indent <> "  " <> BS8.pack (show index) <> ")"
    , emitBehavior (indent <> "    ") commandBehavior
    , indent <> "    ;;"
    ]

emitLoginBehavior :: BS8.ByteString -> LoginBehavior -> BS8.ByteString
emitLoginBehavior indent loginBehavior =
  case loginBehavior of
    LoginCommandBehavior commandBehavior ->
      case commandBehavior of
        CommandSucceeds _ ->
          BS8.unlines
            [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/login-success\""
            , emitBehavior indent commandBehavior
            ]
        CommandFails _ ->
          BS8.unlines
            [ emitBehavior indent commandBehavior
            ]
        CommandArbitrary mkCmd -> mkCmd indent
    LoginRequiresCode ->
      BS8.unlines
        [ indent <> "printf '%s\\n' 'Code is required' 1>&2"
        , indent <> "exit 1"
        ]

emitLogoutBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitLogoutBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\""
        , indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\""
        , indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'"
        , errMessage
        , "EOF"
        , indent <> "exit 1"
        ]
    CommandArbitrary mkCmd -> mkCmd indent

emitConfigBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitConfigBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/configured\""
        , indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'"
        , errMessage
        , "EOF"
        , indent <> "exit 1"
        ]
    CommandArbitrary mkCmd -> mkCmd indent

emitLockBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitLockBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/lock-attempted\""
        , indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/lock-attempted\""
        , indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'"
        , errMessage
        , "EOF"
        , indent <> "exit 1"
        ]
    CommandArbitrary mkCmd -> mkCmd indent

defaultFailingBw :: BwBehavior
defaultFailingBw =
  BwBehavior
    { logoutBehavior = CommandFails ""
    , configServerBehavior = CommandFails ""
    , unlockBehavior = LoginCommandBehavior (CommandFails "credentials were incorrect")
    , listItemsBehaviors = [CommandFails "bw list items failed"]
    , syncBehavior = [CommandFails "bw sync failed"]
    , getPasswordBehavior = CommandFails "bw get password failed"
    , lockBehavior = CommandFails "bw lock failed"
    }

-- Behavior from Bitwarden CLI that allows the server to start
-- listening to the socket
defaultStartingBw :: BwBehavior
defaultStartingBw =
  defaultFailingBw
    { configServerBehavior = CommandSucceeds "config success"
    }

defaultAgentConfig :: AgentConfig
defaultAgentConfig =
  AgentConfig
    { agentBwBehavior = defaultFailingBw
    , agentBwPathMode = UseFakeBwPath
    , agentPathOverride = Nothing
    , agentServerUrl = Nothing
    , agentRefreshIntervalSeconds = Nothing
    }

defaultStartingAgentConfig :: AgentConfig
defaultStartingAgentConfig =
  defaultAgentConfig
    { agentBwBehavior = defaultStartingBw
    }

listItemsPayload :: BS8.ByteString
listItemsPayload =
  BS8.unlines
    [ "["
    , "  {"
    , "    \"id\": \"1\","
    , "    \"name\": \"Battle.net\","
    , "    \"login\": {"
    , "      \"username\": \"joonas_laukka@hotmail.com\""
    , "    }"
    , "  },"
    , "  {"
    , "    \"id\": \"ignored\","
    , "    \"name\": \"Secure note\","
    , "    \"notes\": \"not a login\""
    , "  },"
    , "  {"
    , "    \"id\": \"2\","
    , "    \"name\": \"GitHub\","
    , "    \"login\": {"
    , "      \"username\": \"skyvier\""
    , "    }"
    , "  }"
    , "]"
    ]

-- Keep this summary next to listItemsPayload: the integration assertion is
-- intentionally derived from the same mocked CLI payload.
listItemsSummary :: [Agent.ItemSummary]
listItemsSummary =
  [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com"
  , Agent.ItemSummary "2" "GitHub" "skyvier"
  ]

refreshedListItemsPayload :: BS8.ByteString
refreshedListItemsPayload =
  BS8.unlines
    [ "["
    , "  {"
    , "    \"id\": \"3\","
    , "    \"name\": \"Fastmail\","
    , "    \"login\": {"
    , "      \"username\": \"skyvier@example.com\""
    , "    }"
    , "  }"
    , "]"
    ]

refreshedListItemsSummary :: [Agent.ItemSummary]
refreshedListItemsSummary =
  [ Agent.ItemSummary "3" "Fastmail" "skyvier@example.com"
  ]

assertItemListMatches :: String -> [Agent.ItemSummary] -> Agent.Response -> IO ()
assertItemListMatches message expectedItems response =
  case Agent.responseItems response of
    Just (actualItems, _, _) ->
      assertEqual message expectedItems actualItems
    -- Keep the expected ItemList shape in the failure output when the
    -- response constructor is wrong.
    Nothing ->
      assertEqual message (Agent.itemListResponse expectedItems (Agent.CacheAgeSeconds 0) Agent.CacheRefreshSucceeded) response

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

waitForFileContent :: FilePath -> BS8.ByteString -> IO Bool
waitForFileContent path expectedContent =
  go (60 :: Int)
 where
  go 0 = matches
  go retriesRemaining = do
    contentMatches <- matches
    if contentMatches
      then pure True
      else threadDelay 50000 >> go (retriesRemaining - 1)
  matches = do
    exists <- doesFileExist path
    if exists
      then (== expectedContent) <$> BS8.readFile path
      else pure False

matchesExpectedItems :: [Agent.ItemSummary] -> Agent.Response -> Bool
matchesExpectedItems expectedItems response =
  case Agent.responseItems response of
    Just (actualItems, _, _) -> actualItems == expectedItems
    Nothing -> False

requireAgentExecutable :: IO FilePath
requireAgentExecutable =
  requireAgentExecutableWith
    (lookupEnv "HWARDEN_AGENT_TEST_EXE")
    requireAgentExecutableFromCabal

requireAgentExecutableWith ::
  IO (Maybe FilePath) ->
  IO FilePath ->
  IO FilePath
requireAgentExecutableWith lookupExplicitPath requireFromCabal = do
  explicitPath <- lookupExplicitPath
  case explicitPath of
    Just executablePath -> pure executablePath
    Nothing -> requireFromCabal

requireAgentExecutableFromCabal :: IO FilePath
requireAgentExecutableFromCabal = do
  executablePath <- trimTrailingNewline <$> readProcess "cabal" ["list-bin", "hwarden-agent"] ""
  if null executablePath
    then fail "cabal list-bin returned an empty path for hwarden-agent"
    else pure executablePath

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (== '\n') . reverse

withEnvVarOverride :: String -> Maybe String -> IO a -> IO a
withEnvVarOverride key newValue action = do
  oldValue <- lookupEnv key
  bracket
    (setOverride key newValue)
    (\() -> restoreOverride key oldValue)
    (\() -> action)
 where
  setOverride envKey value =
    case value of
      Just envValue -> setEnv envKey envValue
      Nothing -> unsetEnv envKey
  restoreOverride envKey value =
    case value of
      Just envValue -> setEnv envKey envValue
      Nothing -> unsetEnv envKey

withAgentEnv ::
  AgentConfig ->
  FilePath ->
  FilePath ->
  FilePath ->
  ([(String, String)] -> IO a) ->
  IO a
withAgentEnv agentConfig runtimeDir configDir fakeBwPath cont =
  withServerUrl
    . withRefreshInterval
    . withBwPathMode
    . withPathOverride
    . withConfigHome
    . withEnvVarOverride "XDG_RUNTIME_DIR" (Just runtimeDir)
    $ getEnvironment >>= cont
 where
  withServerUrl =
    withEnvVarOverride
      "HWARDEN_SERVER_URL"
      (agentServerUrl agentConfig)
  withRefreshInterval =
    withEnvVarOverride
      "HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS"
      (show <$> agentRefreshIntervalSeconds agentConfig)
  withBwPathMode =
    case agentBwPathMode agentConfig of
      UseFakeBwPath -> withEnvVarOverride "HWARDEN_BW_PATH" (Just fakeBwPath)
      OmitBwPath -> withEnvVarOverride "HWARDEN_BW_PATH" Nothing
  withPathOverride =
    case agentPathOverride agentConfig of
      Nothing -> id
      Just newPath -> withEnvVarOverride "PATH" (Just newPath)
  withConfigHome =
    withEnvVarOverride "XDG_CONFIG_HOME" (Just configDir)
