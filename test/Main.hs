{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.&.))
import Integration (integrationTests)
import qualified Hwarden.Agent as Agent
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
import Test.QuickCheck (Arbitrary (arbitrary), Property, property)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

newtype MockBitwarden a = MockBitwarden
  { runMockBitwardenInternal :: MockEnv -> a
  }

newtype MockEnv = MockEnv
  { unlockResult :: Either Agent.UnlockError Agent.SessionKey
  }
  deriving (Eq, Show)

instance Arbitrary MockEnv where
  arbitrary = MockEnv <$> arbitrary

instance Functor MockBitwarden where
  fmap f (MockBitwarden run) = MockBitwarden (f . run)

instance Applicative MockBitwarden where
  pure value = MockBitwarden (\_ -> value)
  MockBitwarden apply <*> MockBitwarden run =
    MockBitwarden (\mockEnv -> apply mockEnv (run mockEnv))

instance Monad MockBitwarden where
  MockBitwarden run >>= f =
    MockBitwarden $ \mockEnv ->
      let MockBitwarden next = f (run mockEnv)
       in next mockEnv

instance Bitwarden.Bitwarden MockBitwarden where
  unlock _ _ = MockBitwarden unlockResult

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "hwarden-agent"
    [ parsingTests,
      encodingTests,
      filesystemTests,
      pureStateTransitionTests,
      integrationTests
    ]

parsingTests :: TestTree
parsingTests =
  testGroup
    "parsing"
    [ testCase "request parser decodes unlock payload" $ do
        let payload =
              BS8.pack
                "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "bad-password"))
    , testCase "request parser decodes status payload" $ do
        let payload = BS8.pack "{\"cmd\":\"status\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right Agent.Status
    ]

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "success response encoding matches golden file" $
        assertGoldenEncoding "test/golden/success.json" (Agent.Success "unlocked")
    , testCase "failure response encoding matches golden file" $
        assertGoldenEncoding "test/golden/failure.json" (Agent.Failure "boom")
    ]

filesystemTests :: TestTree
filesystemTests =
  testGroup
    "filesystem"
    [ testCase "prepareSocketDir creates missing directories with owner-only permissions" $ do
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
    ]

pureStateTransitionTests :: TestTree
pureStateTransitionTests =
  testGroup
    "state transitions"
    [ testGroup
        "decide"
        [ testCase "given a locked state, an unlock request triggers an unlock decision" $
            Agent.decide
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret"))
              Agent.Locked
              @?= Agent.Unlock (Agent.Username "me@example.com") (Agent.Password "secret")
        , testCase "given a locked state, a status request replies locked" $
            Agent.decide Agent.Status Agent.Locked
              @?= Agent.Reply (Agent.Success "locked")
        , testCase "given an unlocked state, an unlock request replies already unlocked" $
            Agent.decide
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret"))
              (Agent.Unlocked (Agent.SessionKey "session-key"))
              @?= Agent.Reply (Agent.Success "already unlocked")
        , testProperty "given an unlocked state, a status request replies unlocked" $
            propertyDecideStatusUnlocked
        , testCase "given any state, an unknown request replies with failure" $
            Agent.decide Agent.UnknownRequest Agent.Locked
              @?= Agent.Reply (Agent.Failure "unknown request")
        ]
    , testGroup
        "handleRequestWith"
        [ testProperty "given a locked state, successful unlock action transitions state to unlocked" $
            propertyHandleRequestWithUnlockSuccess
        , testCase "given a locked state, a status request returns locked" $
            let (newState, response) =
                  runMockBitwarden
                    (MockEnv (Left Agent.UnlockUnavailable))
                    (Agent.handleRequestWith Agent.Status Agent.Locked)
             in do
                  newState @?= Agent.Locked
                  response @?= Agent.Success "locked"
        , testProperty "given an unlocked state, a status request returns unlocked" $
            propertyHandleRequestWithStatusUnlocked
        , testProperty "given an unlocked state, an unlock action leaves the state unchanged regardless of the result of the unlock action" $
            propertyHandleRequestWithUnlockedIgnoresUnlockResult
        , testProperty "given any initial state, an unknown request leaves the state unchanged regardless of the result of the unlock action" $
            propertyHandleRequestWithUnknownRequest
        , testProperty "given any state, the encoded status response never exposes the session key" $
            propertyHandleRequestWithStatusDoesNotExposeSessionKey
        ]
    , testGroup
        "handleUnlock"
        [ testProperty "given a locked state, a failed unlock action leaves the state unchanged" $
            propertyHandleUnlockFailure
        , testProperty "given a locked state, successful unlock action transitions state to unlocked" $
            propertyHandleUnlockSuccess
        ]
    ]

propertyHandleRequestWithUnlockSuccess :: Agent.SessionKey -> Property
propertyHandleRequestWithUnlockSuccess sessionKey =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Right sessionKey))
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState == Agent.Unlocked sessionKey
          && response == Agent.Success "unlocked"

propertyDecideStatusUnlocked :: Agent.SessionKey -> Property
propertyDecideStatusUnlocked sessionKey =
  property $
    Agent.decide Agent.Status (Agent.Unlocked sessionKey)
      == Agent.Reply (Agent.Success "unlocked")

propertyHandleRequestWithStatusUnlocked :: Agent.SessionKey -> Property
propertyHandleRequestWithStatusUnlocked sessionKey =
  let currentState = Agent.Unlocked sessionKey
      (newState, response) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable))
          (Agent.handleRequestWith Agent.Status currentState)
   in property $
        newState == currentState
          && response == Agent.Success "unlocked"

propertyHandleRequestWithUnlockedIgnoresUnlockResult :: Agent.SessionKey -> MockEnv -> Property
propertyHandleRequestWithUnlockedIgnoresUnlockResult sessionKey mockEnv =
  let currentState = Agent.Unlocked sessionKey
      (newState, response) =
        runMockBitwarden
          mockEnv
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) currentState)
   in property $
        newState == currentState
          && response == Agent.Success "already unlocked"

propertyHandleRequestWithUnknownRequest :: Agent.AgentState -> MockEnv -> Property
propertyHandleRequestWithUnknownRequest initialState mockEnv =
  let (newState, response) =
        runMockBitwarden mockEnv (Agent.handleRequestWith Agent.UnknownRequest initialState)
   in property $
        newState == initialState
          && response == Agent.Failure "unknown request"

propertyHandleRequestWithStatusDoesNotExposeSessionKey :: Agent.AgentState -> Property
propertyHandleRequestWithStatusDoesNotExposeSessionKey initialState =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable))
          (Agent.handleRequestWith Agent.Status initialState)
   in property $
        newState == initialState
          && statusResponseMatchesState initialState response
          && not (statusResponseLeaksSessionKey initialState response)

propertyHandleUnlockFailure :: Agent.UnlockError -> Property
propertyHandleUnlockFailure unlockError =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Left unlockError))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Locked
          && response == expectedFailure unlockError
          && not (encodedResponseContains "secret" response)

propertyHandleUnlockSuccess :: Agent.SessionKey -> Property
propertyHandleUnlockSuccess sessionKey =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Right sessionKey))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Unlocked sessionKey
          && response == Agent.Success "unlocked"

expectedFailure :: Agent.UnlockError -> Agent.Response
expectedFailure Agent.UnlockUnavailable = Agent.Failure "bw login failed"
expectedFailure (Agent.UnlockFailed err) = Agent.Failure (Agent.sanitizeError (Agent.Password "secret") err)

statusResponseMatchesState :: Agent.AgentState -> Agent.Response -> Bool
statusResponseMatchesState Agent.Locked response = response == Agent.Success "locked"
statusResponseMatchesState (Agent.Unlocked _) response = response == Agent.Success "unlocked"

statusResponseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
statusResponseLeaksSessionKey Agent.Locked _ = False
statusResponseLeaksSessionKey (Agent.Unlocked (Agent.SessionKey sessionKey)) response =
  BS8.pack sessionText `BS.isInfixOf` encodedResponse response
  where
    sessionText = show sessionKey

runMockBitwarden :: MockEnv -> MockBitwarden a -> a
runMockBitwarden mockEnv (MockBitwarden run) = run mockEnv

encodedResponseContains :: String -> Agent.Response -> Bool
encodedResponseContains needle response =
  BS8.pack needle `BS.isInfixOf` encodedResponse response

encodedResponse :: Agent.Response -> BS.ByteString
encodedResponse = LBS.toStrict . Aeson.encode

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

createTempDir :: String -> IO FilePath
createTempDir prefix = do
  tempBase <- getTemporaryDirectory
  (tempPath, tempHandle) <- openTempFile tempBase prefix
  hClose tempHandle
  removeFile tempPath
  createDirectoryIfMissing True tempPath
  pure tempPath
