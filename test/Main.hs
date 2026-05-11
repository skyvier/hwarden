{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Network.Socket
  ( Family (AF_UNIX),
    ShutdownCmd (ShutdownSend),
    SockAddr (SockAddrUnix),
    Socket,
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
    createDirectory,
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
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

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
      [ testCase "unlock returns invalid credentials" $ do
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

recvAll :: Socket -> IO BS8.ByteString
recvAll conn = go []
  where
    go acc = do
      chunk <- NBS.recv conn 4096
      if BS8.null chunk
        then pure (BS8.concat (reverse acc))
        else go (chunk : acc)

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
  createDirectory tempPath
  pure tempPath
