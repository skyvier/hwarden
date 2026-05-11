{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.&.))
import Data.Text (Text)
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
    doesPathExist,
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
import System.Posix.Files
  ( fileMode,
    getFileStatus,
    setFileMode
  )
import System.Process
  ( CreateProcess (cwd, env, std_err, std_out),
    ProcessHandle,
    StdStream (Inherit),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess
  )
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

data Response = Response
  { ok :: Bool,
    message :: Maybe Text,
    err :: Maybe Text
  }

data AgentResource = AgentResource
  { socketPath :: FilePath,
    processHandle :: ProcessHandle,
    tempRoot :: FilePath
  }

instance Aeson.FromJSON Response where
  parseJSON = Aeson.withObject "Response" $ \obj ->
    Response <$> obj Aeson..: "ok" <*> obj Aeson..:? "message" <*> obj Aeson..:? "error"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  withResource setupAgent cleanupAgent $ \getAgent ->
    testGroup
      "hwarden-agent"
      [ testCase "request parser decodes unlock payload" $ do
          let payload =
                BS8.pack
                  "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"
          Aeson.eitherDecodeStrict' payload
            @?= Right (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
      , testCase "success response encoding matches golden file" $
          assertGoldenEncoding "test/golden/success.json" (Agent.Success "unlocked")
      , testCase "failure response encoding matches golden file" $
          assertGoldenEncoding "test/golden/failure.json" (Agent.Failure "boom")
      , testCase "prepareSocketDir creates missing directories with owner-only permissions" $ do
          root <- createTempDir "hwarden-agent-test"
          let socketDir = root </> "nested" </> "runtime" </> "hwarden"
          Agent.prepareSocketDir socketDir
          exists <- doesPathExist socketDir
          assertBool "socket directory should exist" exists
          assertDirectoryOwnerOnly socketDir
          removeDirectoryRecursive root
      , testCase "prepareSocketDir tightens existing directory permissions" $ do
          root <- createTempDir "hwarden-agent-test"
          let socketDir = root </> "hwarden"
          createDirectoryIfMissing True socketDir
          setFileMode socketDir 0o755
          Agent.prepareSocketDir socketDir
          assertDirectoryOwnerOnly socketDir
          removeDirectoryRecursive root
      , testCase "removeExistingSocket deletes an existing file" $ do
          root <- createTempDir "hwarden-agent-test"
          let staleSocketPath = root </> "agent.sock"
          BS.writeFile staleSocketPath ""
          Agent.removeExistingSocket staleSocketPath
          exists <- doesPathExist staleSocketPath
          assertBool "socket file should be deleted" (not exists)
          removeDirectoryRecursive root
      , testCase "removeExistingSocket succeeds when directory exists but socket file does not" $ do
          root <- createTempDir "hwarden-agent-test"
          let missingSocketPath = root </> "agent.sock"
          Agent.removeExistingSocket missingSocketPath
          exists <- doesPathExist missingSocketPath
          assertBool "socket file should remain absent" (not exists)
          removeDirectoryRecursive root
      , testCase "removeExistingSocket succeeds when directory and socket file do not exist" $ do
          root <- createTempDir "hwarden-agent-test"
          let missingSocketPath = root </> "missing" </> "agent.sock"
          Agent.removeExistingSocket missingSocketPath
          exists <- doesPathExist missingSocketPath
          assertBool "socket file should remain absent" (not exists)
          removeDirectoryRecursive root
      , testCase "unlock returns invalid credentials" $ do
          agent <- getAgent
          response <- sendUnlock (socketPath agent)
          assertBool "expected failure response" (not (ok response))
          assertEqual "failure should not include success message" Nothing (message response)
          assertEqual
            "expected invalid credentials error"
            (Just "credentials were incorrect")
            (err response)
      ]

setupAgent :: IO AgentResource
setupAgent = do
  tmpDir <- createTempDir "hwarden-agent-test"
  let runtimeDir = tmpDir </> "runtime"
      fakeBinDir = tmpDir </> "bin"
      agentSocketPath = runtimeDir </> "hwarden" </> "agent.sock"
  createDirectoryIfMissing True runtimeDir
  createDirectoryIfMissing True fakeBinDir
  writeFakeBw fakeBinDir
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

sendUnlock :: FilePath -> IO Response
sendUnlock agentSocketPath =
  bracket open close $ \conn -> do
    NBS.sendAll conn requestBody
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
    requestBody =
      BS8.pack "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"

writeFakeBw :: FilePath -> IO ()
writeFakeBw fakeBinDir = do
  let fakeBw = fakeBinDir </> "bw"
  BS8.writeFile
    fakeBw
    ( BS8.unlines
        [ "#!/bin/sh",
          "echo 'credentials were incorrect' 1>&2",
          "exit 1"
        ]
    )
  permissions <- getPermissions fakeBw
  setPermissions fakeBw permissions {executable = True}

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

assertGoldenEncoding :: FilePath -> Agent.Response -> IO ()
assertGoldenEncoding goldenPath response = do
  expected <- BS.readFile goldenPath
  let normalizedExpected = BS8.dropWhileEnd (== '\n') expected
  LBS.toStrict (Aeson.encode response) @?= normalizedExpected

assertDirectoryOwnerOnly :: FilePath -> IO ()
assertDirectoryOwnerOnly path = do
  status <- getFileStatus path
  let permissionBits = fileMode status .&. 0o777
  assertEqual "directory mode should be 0700" 0o700 permissionBits
