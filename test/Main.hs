{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.&.))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Test.QuickCheck (Arbitrary (arbitrary), Property, property, (==>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

newtype MockBitwarden a = MockBitwarden
  { runMockBitwardenInternal :: MockEnv -> a
  }

data MockEnv = MockEnv
  { unlockResult :: Either Agent.UnlockError Agent.SessionKey,
    listItemsResult :: Either Agent.ListItemsError [Agent.ItemSummary],
    getPasswordResult :: Either Bitwarden.GetPasswordError Agent.PasswordValue
  }
  deriving (Eq, Show)

instance Arbitrary MockEnv where
  arbitrary = MockEnv <$> arbitrary <*> arbitrary <*> arbitrary

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
  listItems _ = MockBitwarden listItemsResult
  getPassword _ _ = MockBitwarden getPasswordResult

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
    , testCase "determineBitwardenServerUrl uses the EU default when unset" $
        Bitwarden.determineBitwardenServerUrl Nothing
          @?= Bitwarden.defaultBitwardenServerUrl
    , testCase "determineBitwardenServerUrl uses the override when set" $
        Bitwarden.determineBitwardenServerUrl (Just "https://vault.example.test")
          @?= "https://vault.example.test"
    , testCase "bitwarden item parser decodes a login item" $ do
        let payload =
              BS8.pack
                "[{\"id\":\"1\",\"name\":\"Battle.net\",\"login\":{\"username\":\"skyvier\"}}]"
        Aeson.eitherDecodeStrict payload
          @?= Right [
            Bitwarden.BwItem 
              "1" 
              "Battle.net" 
              (Just $ Bitwarden.BwLogin "skyvier")
            ]
    , testCase "bitwarden item parser decodes non-login items too" $ do
        let payload =
              BS8.pack
                "[{\"id\":\"1\",\"name\":\"Secure note\"}]"
        Aeson.eitherDecodeStrict payload
          @?= Right [
            Bitwarden.BwItem 
              "1" 
              "Secure note" 
              Nothing
            ]
    , testCase "request parser decodes status payload" $ do
        let payload = BS8.pack "{\"cmd\":\"status\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right Agent.Status
    , testCase "request parser decodes list-items payload" $ do
        let payload = BS8.pack "{\"cmd\":\"list-items\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right Agent.ListItems
    , testCase "request parser decodes get-password payload" $ do
        let payload = BS8.pack "{\"cmd\":\"get-password\",\"id\":\"item-123\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right (Agent.GetPasswordRequest "item-123")
    ]

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "success response encoding matches golden file" $
        assertGoldenEncoding "test/golden/success.json" (Agent.Success "unlocked")
    , testCase "failure response encoding matches golden file" $
        assertGoldenEncoding "test/golden/failure.json" (Agent.Failure "boom")
    , testCase "item-list response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/item-list.json"
          ( Agent.ItemList
              [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com",
                Agent.ItemSummary "2" "GitHub" "skyvier"
              ]
          )
    , testCase "password response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/password-result.json"
          (Agent.PasswordResult "item-123" (Agent.PasswordValue "super-secret"))
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
        , testCase "given a locked state, a list-items request replies locked failure" $
            Agent.decide Agent.ListItems Agent.Locked
              @?= Agent.Reply (Agent.Failure "locked")
        , testProperty "given a locked state, a get-password request replies locked failure" $
            propertyDecideGetPasswordLocked
        , testCase "given an unlocked state, an unlock request replies already unlocked" $
            Agent.decide
              (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret"))
              (Agent.Unlocked (Agent.SessionKey "session-key"))
              @?= Agent.Reply (Agent.Success "already unlocked")
        , testProperty "given an unlocked state, a status request replies unlocked" $
            propertyDecideStatusUnlocked
        , testProperty "given an unlocked state, a list-items request triggers item listing" $
            propertyDecideListItemsUnlocked
        , testProperty "given an unlocked state, a get-password request triggers password retrieval for the requested id" $
            propertyDecideGetPasswordUnlocked
        , testCase "given any state, an unknown request replies with failure" $
            Agent.decide Agent.UnknownRequest Agent.Locked
              @?= Agent.Reply (Agent.Failure "unknown request")
        ]
    , testGroup
        "handleRequestWith"
        [ testProperty "given a locked state, successful unlock action transitions state to unlocked" $
            propertyHandleRequestWithUnlockSuccess
        , testCase "given a locked state, a status request returns locked" $
            let currentState = Agent.Locked
                (newState, response) =
                  runMockBitwarden
                    (MockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
                    (Agent.handleRequestWith Agent.Status currentState)
             in do
                  newState @?= currentState
                  response @?= Agent.Success "locked"
        , testProperty "given an unlocked state, a status request returns unlocked" $
            propertyHandleRequestWithStatusUnlocked
        , testCase "given a locked state, a list-items request returns locked failure" $
            let currentState = Agent.Locked
                (newState, response) =
                  runMockBitwarden
                    (MockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
                    (Agent.handleRequestWith Agent.ListItems currentState)
             in do
                  newState @?= currentState
                  response @?= Agent.Failure "locked"
        , testCase "given an unlocked state, a list-items request returns items and preserves state" $
            let items =
                  [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com",
                    Agent.ItemSummary "2" "GitHub" "skyvier"
                  ]
                currentState = Agent.Unlocked (Agent.SessionKey "session-key")
                (newState, response) =
                  runMockBitwarden
                    (MockEnv (Left Agent.UnlockUnavailable) (Right items) (Left Bitwarden.GetPasswordUnavailable))
                    (Agent.handleRequestWith Agent.ListItems currentState)
             in do
                  newState @?= currentState
                  response @?= Agent.ItemList items
        , testProperty "given any initial state, get-password preserves state regardless of the backend result" $
            propertyHandleRequestWithGetPasswordPreservesState
        , testProperty "given an unlocked state, an unlock action leaves the state unchanged regardless of the result of the unlock action" $
            propertyHandleRequestWithUnlockedIgnoresUnlockResult
        , testProperty "given any initial state, an unknown request leaves the state unchanged regardless of the result of the unlock action" $
            propertyHandleRequestWithUnknownRequest
        , testProperty "given any state, the encoded status response never exposes the session key" $
            propertyHandleRequestWithStatusDoesNotExposeSessionKey
        , testProperty "given an unlocked state, the encoded list-items response never exposes the session key" $
            propertyHandleRequestWithListItemsDoesNotExposeSessionKey
        , testProperty "given an unlocked state, a get-password failure response never exposes the session key" $
            propertyHandleRequestWithGetPasswordFailureDoesNotExposeSessionKey
        , testProperty "given a successful get-password response, show never exposes the plaintext password" $
            propertyPasswordResultShowDoesNotExposePassword
        ]
    , testGroup
        "handleListItems"
        [ testProperty "given any initial state, handleListItems never changes the agent state" $
            propertyHandleListItemsPreservesState
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
          (MockEnv (Right sessionKey) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState == Agent.Unlocked sessionKey
          && response == Agent.Success "unlocked"

propertyDecideGetPasswordLocked :: String -> Property
propertyDecideGetPasswordLocked itemId =
  property $
    Agent.decide (Agent.GetPasswordRequest (T.pack itemId)) Agent.Locked
      == Agent.Reply (Agent.Failure "locked")

propertyDecideStatusUnlocked :: Agent.SessionKey -> Property
propertyDecideStatusUnlocked sessionKey =
  property $
    Agent.decide Agent.Status (Agent.Unlocked sessionKey)
      == Agent.Reply (Agent.Success "unlocked")

propertyDecideListItemsUnlocked :: Agent.SessionKey -> Property
propertyDecideListItemsUnlocked sessionKey =
  property $
    Agent.decide Agent.ListItems (Agent.Unlocked sessionKey)
      == Agent.ListItemsAction sessionKey

propertyDecideGetPasswordUnlocked :: Agent.SessionKey -> String -> Property
propertyDecideGetPasswordUnlocked sessionKey itemId =
  property $
    Agent.decide (Agent.GetPasswordRequest itemIdText) (Agent.Unlocked sessionKey)
      == Agent.GetPasswordAction sessionKey itemIdText
  where
    itemIdText = T.pack itemId

propertyHandleRequestWithStatusUnlocked :: Agent.SessionKey -> Property
propertyHandleRequestWithStatusUnlocked sessionKey =
  let currentState = Agent.Unlocked sessionKey
      (newState, response) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.Status currentState)
   in property $
        newState == currentState
          && response == Agent.Success "unlocked"

propertyHandleRequestWithGetPasswordPreservesState ::
  Agent.AgentState ->
  String ->
  Either Bitwarden.GetPasswordError Agent.PasswordValue ->
  Property
propertyHandleRequestWithGetPasswordPreservesState initialState itemId mockGetPasswordResult =
  let (newState, _) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable) (Right []) mockGetPasswordResult)
          (Agent.handleRequestWith (Agent.GetPasswordRequest (T.pack itemId)) initialState)
   in property (newState == initialState)

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
          (MockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.Status initialState)
   in stateUsesNonEmptySessionKey initialState ==>
        property
          ( newState == initialState
              && statusResponseMatchesState initialState response
              && not (statusResponseLeaksSessionKey initialState response)
          )

propertyHandleRequestWithListItemsDoesNotExposeSessionKey :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleRequestWithListItemsDoesNotExposeSessionKey sessionKey items =
  let currentState = Agent.Unlocked sessionKey
      (newState, response) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable) (Right items) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in doesNotContainSessionKey sessionText items ==>
        property
          ( newState == currentState
              && response == Agent.ItemList items
              && not (TE.encodeUtf8 sessionText `BS.isInfixOf` encodedResponse response)
          )
  where
    Agent.SessionKey sessionText = sessionKey

propertyHandleRequestWithGetPasswordFailureDoesNotExposeSessionKey ::
  String ->
  String ->
  Property
propertyHandleRequestWithGetPasswordFailureDoesNotExposeSessionKey itemId suffix =
  let sessionText = "session-needle-" <> T.pack suffix <> "-end"
      sessionKey = Agent.SessionKey sessionText
      currentState = Agent.Unlocked sessionKey
      leakingError = Bitwarden.GetPasswordFailed ("prefix " <> sessionText <> " suffix")
      (newState, response) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable) (Right []) (Left leakingError))
          (Agent.handleRequestWith (Agent.GetPasswordRequest (T.pack itemId)) currentState)
   in property
        ( newState == currentState
            && isFailure response
            && not (sessionKeyAppearsInEncodedResponse sessionKey response)
            && encodedResponseContains "<redacted>" response
        )

propertyPasswordResultShowDoesNotExposePassword :: String -> String -> Property
propertyPasswordResultShowDoesNotExposePassword itemId passwordText =
  not (null passwordText) ==>
    property
      ( let passwordNeedle =
              T.pack ("pw-needle-" <> passwordText <> "-end")
            response = Agent.PasswordResult (T.pack itemId) (Agent.PasswordValue passwordNeedle)
            rendered = T.pack (show response)
         in rendered /= passwordNeedle
              && not (passwordNeedle `T.isInfixOf` rendered)
      )

propertyHandleUnlockFailure :: Agent.UnlockError -> Property
propertyHandleUnlockFailure unlockError =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Left unlockError) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Locked
          && response == expectedFailure unlockError
          && not (encodedResponseContains "secret" response)

propertyHandleListItemsPreservesState ::
  Agent.SessionKey ->
  Agent.AgentState ->
  Either Agent.ListItemsError [Agent.ItemSummary] ->
  Property
propertyHandleListItemsPreservesState sessionKey initialState mockListItemsResult =
  let (newState, _) =
        runMockBitwarden
          (MockEnv (Left Agent.UnlockUnavailable) mockListItemsResult (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleListItems sessionKey initialState)
   in property (newState == initialState)

propertyHandleUnlockSuccess :: Agent.SessionKey -> Property
propertyHandleUnlockSuccess sessionKey =
  let (newState, response) =
        runMockBitwarden
          (MockEnv (Right sessionKey) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Unlocked sessionKey
          && response == Agent.Success "unlocked"

expectedFailure :: Agent.UnlockError -> Agent.Response
expectedFailure Agent.UnlockUnavailable = Agent.Failure "bw login failed"
expectedFailure (Agent.UnlockFailed err) = Agent.Failure (Agent.sanitizeUnlockError (Agent.Password "secret") err)

statusResponseMatchesState :: Agent.AgentState -> Agent.Response -> Bool
statusResponseMatchesState Agent.Locked response = response == Agent.Success "locked"
statusResponseMatchesState (Agent.Unlocked _) response = response == Agent.Success "unlocked"

statusResponseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
statusResponseLeaksSessionKey Agent.Locked _ = False
statusResponseLeaksSessionKey (Agent.Unlocked (Agent.SessionKey sessionKey)) response =
  TE.encodeUtf8 sessionKey `BS.isInfixOf` encodedResponse response

sessionKeyAppearsInEncodedResponse :: Agent.SessionKey -> Agent.Response -> Bool
sessionKeyAppearsInEncodedResponse (Agent.SessionKey sessionKey) response =
  TE.encodeUtf8 sessionKey `BS.isInfixOf` encodedResponse response

isFailure :: Agent.Response -> Bool
isFailure (Agent.Failure _) = True
isFailure _ = False

runMockBitwarden :: MockEnv -> MockBitwarden a -> a
runMockBitwarden mockEnv (MockBitwarden run) = run mockEnv

encodedResponseContains :: String -> Agent.Response -> Bool
encodedResponseContains needle response =
  BS8.pack needle `BS.isInfixOf` encodedResponse response

encodedResponse :: Agent.Response -> BS.ByteString
encodedResponse = LBS.toStrict . Aeson.encode

doesNotContainSessionKey :: Text -> [Agent.ItemSummary] -> Bool
doesNotContainSessionKey sessionText items =
  not (T.null sessionText)
    && not (TE.encodeUtf8 sessionText `BS.isInfixOf` encodedResponse (Agent.ItemList []))
    && all itemDoesNotContainSessionKey items
  where
    encodedSessionKey = TE.encodeUtf8 sessionText
    itemDoesNotContainSessionKey item =
      not (encodedSessionKey `BS.isInfixOf` LBS.toStrict (Aeson.encode item))

stateUsesNonEmptySessionKey :: Agent.AgentState -> Bool
stateUsesNonEmptySessionKey Agent.Locked = True
stateUsesNonEmptySessionKey (Agent.Unlocked (Agent.SessionKey sessionText)) =
  not (T.null sessionText)
    && not (TE.encodeUtf8 sessionText `BS.isInfixOf` encodedResponse (Agent.Success "unlocked"))

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
