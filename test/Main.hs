{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Bits ((.&.))
import qualified Hwarden.Agent as Agent
import qualified Hwarden.App as App
import qualified Hwarden.Bitwarden as Bitwarden
import System.Directory
  ( createDirectoryIfMissing,
    doesPathExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile
  )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files
  ( fileMode,
    getFileStatus,
    setFileMode
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import qualified Test.Sanitization as Sanitization
import qualified Test.Cache as Cache
import qualified Test.Logging as Logging
import qualified Test.Agent.Decide as Agent.Decide
import qualified Test.StateMachine as StateMachine
import qualified Test.RequestHandler as RequestHandler
import qualified Test.JsonCodec as JsonCodec
import qualified Test.Integration as Integration

  
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "hwarden-agent"
    [ 
      filesystemTests,

      Sanitization.tests,
      Cache.tests,
      Logging.tests,
      Agent.Decide.tests,
      StateMachine.tests,
      RequestHandler.tests,
      JsonCodec.tests,
      Integration.tests,

      bitwardenServerUrlTests,
      bitwardenCliPathTests
    ]


filesystemTests :: TestTree
filesystemTests =
  testGroup
    "filesystem"
    [ testCase "prepareRuntimeDir creates missing directories with owner-only permissions" $ do
        root <- createTempDir "hwarden-agent-test"
        let socketDir = root </> "nested" </> "runtime" </> "hwarden"
        Agent.prepareRuntimeDir socketDir
        exists <- doesPathExist socketDir
        assertBool "socket directory should exist" exists
        assertDirectoryOwnerOnly socketDir
        removeDirectoryRecursive root
    , testCase "prepareRuntimeDir tightens existing directory permissions" $ do
        root <- createTempDir "hwarden-agent-test"
        let socketDir = root </> "hwarden"
        createDirectoryIfMissing True socketDir
        setFileMode socketDir 0o755
        Agent.prepareRuntimeDir socketDir
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
    ]


bitwardenServerUrlTests :: TestTree
bitwardenServerUrlTests = testGroup "determineBitwardenServerUrl"
    [ testCase "determineBitwardenServerUrl uses the EU default when unset" $
        Bitwarden.determineBitwardenServerUrl Nothing
          @?= Bitwarden.defaultBitwardenServerUrl
    , testCase "determineBitwardenServerUrl uses the override when set" $
        Bitwarden.determineBitwardenServerUrl (Just "https://vault.example.test")
          @?= "https://vault.example.test"
    ]

bitwardenCliPathTests :: TestTree
bitwardenCliPathTests = testGroup "Bitwarden CLI path"
  [ testGroup "parseBitwardenCliPath"
    [ testCase "parseBitwardenCliPath returns the configured path" $
      App.parseBitwardenCliPath (Just "/nix/store/test-bw/bin/bw")
        @?= Right "/nix/store/test-bw/bin/bw"
    , testCase "parseBitwardenCliPath fails when the path is missing" $
        App.parseBitwardenCliPath Nothing
          @?= Left "HWARDEN_BW_PATH is not set"
    , testCase "parseBitwardenCliPath fails when the path is empty" $
        App.parseBitwardenCliPath (Just "")
          @?= Left "HWARDEN_BW_PATH is empty"
    ]
  , testGroup "validateBitwardenCliPath"
    [ testCase "validateBitwardenCliPath accepts an executable file" $ do
        root <- createTempDir "hwarden-agent-test"
        let cliPath = root </> "bw"
        BS.writeFile cliPath ""
        setFileMode cliPath 0o700
        validationResult <- App.validateBitwardenCliPath cliPath
        removeDirectoryRecursive root
        validationResult @?= Right cliPath
    , testCase "validateBitwardenCliPath fails when the path does not exist" $
        App.validateBitwardenCliPath "/definitely/missing/bw"
          >>= (@?= Left "HWARDEN_BW_PATH does not exist")
    , testCase "validateBitwardenCliPath fails when the path is not executable" $ do
        root <- createTempDir "hwarden-agent-test"
        let cliPath = root </> "bw"
        BS.writeFile cliPath ""
        setFileMode cliPath 0o600
        validationResult <- App.validateBitwardenCliPath cliPath
        removeDirectoryRecursive root
        validationResult @?= Left "HWARDEN_BW_PATH is not executable"
    ]
  ]

assertDirectoryOwnerOnly :: FilePath -> IO ()
assertDirectoryOwnerOnly path = do
  status <- getFileStatus path
  let permissionBits = fileMode status .&. 0o777
  assertEqual "directory mode should be 0700" 0o700 permissionBits

createTempDir :: String -> IO FilePath
createTempDir prefix = do
  tempBase <- getTemporaryDirectory
  (tempPath, tempHandle) <- openTempFile tempBase prefix
  hClose tempHandle
  removeFile tempPath
  createDirectoryIfMissing True tempPath
  pure tempPath
