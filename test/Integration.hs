{-# LANGUAGE OverloadedStrings #-}

module Integration (integrationTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Hwarden.Agent as Agent
import Hwarden.Socket (recvAll)
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
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process
  ( CreateProcess (cwd, env, std_err, std_out),
    ProcessHandle,
    StdStream (Inherit),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

data BwBehavior
  = BwSucceeds BS8.ByteString
  | BwFails BS8.ByteString

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
        agent <- setupAgent (BwFails "credentials were incorrect")
        response <- sendRequest (socketPath agent) Agent.Status
        cleanupAgent agent
        assertEqual
          "expected locked status response"
          (Agent.Success "locked")
          response
    , testCase "sending status then successful unlock then status via the socket reports locked then unlocked" $ do
        agent <- setupAgent (BwSucceeds "session-key-123")
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
    , testCase "sending status then failed unlock then status via the socket reports locked then still locked" $ do
        agent <- setupAgent (BwFails "credentials were incorrect")
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
    , testCase "sending invalid credentials via the socket results in failure message" $ do
        agent <- setupAgent (BwFails "credentials were incorrect")
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

setupAgent :: BwBehavior -> IO AgentResource
setupAgent bwBehavior = do
  tmpDir <- createTempDir "hwarden-agent-test"
  let runtimeDir = tmpDir </> "runtime"
      fakeBinDir = tmpDir </> "bin"
      agentSocketPath = runtimeDir </> "hwarden" </> "agent.sock"
  createDirectoryIfMissing True runtimeDir
  createDirectoryIfMissing True fakeBinDir
  writeFakeBw fakeBinDir bwBehavior
  hwardenAgent <- requireExecutable "hwarden-agent"
  bwReal <- requireExecutable "bw"
  baseEnv <- getEnvironment
  let pathValue = fakeBinDir <> ":" <> takeDirectory bwReal
      agentEnv = setEnvVar "PATH" pathValue (setEnvVar "XDG_RUNTIME_DIR" runtimeDir baseEnv)

  handle <- spawnAgent hwardenAgent tmpDir agentEnv
  waitForSocket agentSocketPath
  pure
    AgentResource
      { socketPath = agentSocketPath,
        processHandle = handle,
        tempRoot = tmpDir
      }

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

waitForSocket :: FilePath -> IO ()
waitForSocket agentSocketPath = go (200 :: Int)
  where
    go 0 = fail ("socket was not created: " <> agentSocketPath)
    go retries = do
      exists <- doesFileExist agentSocketPath
      if exists
        then pure ()
        else threadDelay 50000 >> go (retries - 1)

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

writeFakeBw :: FilePath -> BwBehavior -> IO ()
writeFakeBw fakeBinDir bwBehavior = do
  let fakeBw = fakeBinDir </> "bw"
  BS8.writeFile
    fakeBw
    (scriptFor bwBehavior)
  permissions <- getPermissions fakeBw
  setPermissions fakeBw permissions {executable = True}

scriptFor :: BwBehavior -> BS8.ByteString
scriptFor bwBehavior =
  case bwBehavior of
    BwSucceeds sessionKey ->
      BS8.unlines
        [ "#!/bin/sh",
          "printf '%s\\n' \"" <> sessionKey <> "\"",
          "exit 0"
        ]
    BwFails errMessage ->
      BS8.unlines
        [ "#!/bin/sh",
          "printf '%s\\n' \"" <> errMessage <> "\" 1>&2",
          "exit 1"
        ]

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
