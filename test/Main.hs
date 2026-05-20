{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Time (MonadTime (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.&.))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, addUTCTime)
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
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    property,
    (==>)
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

newtype MockBitwarden a = MockBitwarden
  { runMockBitwardenInternal :: MockEnv -> a
  }

data MockEnv = MockEnv
  { unlockResult :: Either Agent.UnlockError Agent.SessionKey,
    listItemsResult :: Either Agent.ListItemsError [Agent.ItemSummary],
    getPasswordResult :: Either Bitwarden.GetPasswordError Agent.PasswordValue,
    mockCurrentTime :: UTCTime
  }
  deriving (Eq, Show)

instance Arbitrary MockEnv where
  arbitrary = MockEnv <$> arbitrary <*> arbitrary <*> arbitrary <*> pure mockNow

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

instance MonadTime MockBitwarden where
  currentTime = MockBitwarden mockCurrentTime
  monotonicTime = pure 0

mockNow :: UTCTime
mockNow = read "2026-05-20 12:00:05 UTC"

mkMockEnv ::
  Either Agent.UnlockError Agent.SessionKey ->
  Either Agent.ListItemsError [Agent.ItemSummary] ->
  Either Bitwarden.GetPasswordError Agent.PasswordValue ->
  MockEnv
mkMockEnv mockUnlockResult mockListItemsResult mockGetPasswordResult =
  MockEnv mockUnlockResult mockListItemsResult mockGetPasswordResult mockNow

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
          @?= Right (Agent.GetPasswordRequest (Agent.LoginItemId "item-123"))
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
              (Agent.CacheAgeSeconds 0)
          )
    , testCase "password response encoding matches golden file" $
        assertGoldenEncoding
          "test/golden/password-result.json"
          (Agent.PasswordResult (Agent.LoginItemId "item-123") (Agent.PasswordValue "super-secret"))
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
              (Agent.Unlocked (Agent.SessionKey "session-key") Agent.CacheNotYetFilled)
              @?= Agent.Reply (Agent.Success "already unlocked")
        , testProperty "given an unlocked state, a status request replies unlocked" $
            propertyDecideStatusUnlocked
        , testProperty "given an unlocked state with cached items, a list-items request triggers a cached list action" $
            propertyDecideListItemsUnlocked
        , testCase "given an unlocked state with a not-yet-filled cache, a list-items request replies with cache unavailable" $
            Agent.decide
              Agent.ListItems
              (Agent.Unlocked (Agent.SessionKey "session-key") Agent.CacheNotYetFilled)
              @?= Agent.Reply (Agent.Failure "item cache unavailable")
        , testProperty "given an unlocked state with a failed cache fill, a list-items request replies with cache unavailable" $
            propertyDecideListItemsFailedCacheFill
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
        , testProperty "given a locked state, a successful unlock with a failed initial item cache fill still returns unlocked and records the cache failure" $
            propertyHandleRequestWithUnlockCacheFillFailure
        , testCase "given a locked state, a status request returns locked" $
            let currentState = Agent.Locked
                (newState, response, effects) =
                  runMockBitwarden
                    (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
                    (Agent.handleRequestWith Agent.Status currentState)
             in do
                  newState @?= currentState
                  response @?= Agent.Success "locked"
                  effects @?= []
        , testProperty "given an unlocked state, a status request returns unlocked" $
            propertyHandleRequestWithStatusUnlocked
        , testCase "given a locked state, a list-items request returns locked failure" $
            let currentState = Agent.Locked
                (newState, response, effects) =
                  runMockBitwarden
                    (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
                    (Agent.handleRequestWith Agent.ListItems currentState)
             in do
                  newState @?= currentState
                  response @?= Agent.Failure "locked"
                  effects @?= []
        , testProperty "given an unlocked state with a not-yet-filled cache, a list-items request returns cache unavailable and preserves state" $
            propertyHandleRequestWithListItemsNotYetFilled
        , testProperty "given an unlocked state with a failed cache fill, a list-items request returns cache unavailable and preserves state" $
            propertyHandleRequestWithListItemsFailedCacheFill
        , testProperty "given an unlocked state with cached items, a list-items request returns items and preserves state" $
            propertyHandleRequestWithListItemsPreservesState
        , testProperty "given cached items refreshed N seconds ago, a list-items request reports age N exactly" $
            propertyHandleRequestWithListItemsReportsExactCacheAge
        , testProperty "given any initial state, get-password preserves state regardless of the backend result" $
            propertyHandleRequestWithGetPasswordPreservesState
        , testProperty "given a locked state, a get-password response indicates that it failed due to locked state" $
            propertyHandleRequestWithGetPasswordLocked
        , testProperty "given an unlocked state, a successful get-password response returns the password as is" $
            propertyHandleRequestWithGetPasswordSuccess
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
        , testProperty "a refresh loop effect is only emitted by a successful unlock from the locked state" $
            propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop
        ]
    , testGroup
        "handleListItems"
        [ testProperty "given any initial state, handleListItems never changes the agent state" $
            propertyHandleListItemsPreservesState
        , testProperty "given a cache entry refreshed N seconds ago, handleListItems reports age N exactly" $
            propertyHandleListItemsReportsExactCacheAge
        ]
    , testGroup
        "handleUnlock"
        [ testProperty "given a locked state, a failed unlock action leaves the state unchanged" $
            propertyHandleUnlockFailure
        , testProperty "given a locked state, successful unlock action transitions state to unlocked" $
            propertyHandleUnlockSuccess
        , testProperty "given a successful unlock and a failed initial item cache fill, unlock still succeeds and records the cache failure" $
            propertyHandleUnlockCacheFillFailure
        ]
    , testGroup
        "cacheAgeSeconds"
        [ testProperty "given a cache entry refreshed N seconds ago, cacheAgeSeconds returns N exactly" $
            propertyCacheAgeSecondsIsExact
        ]
    , testGroup
        "updateItemCacheState"
        [ testProperty "given any previous cache state, a successful refresh replaces it with a ready cache and success status" $
            propertyUpdateItemCacheStateSuccessReplacesState
        , testProperty "given a stale ready cache, a successful refresh replaces both cached items and stale failure metadata" $
            propertyUpdateItemCacheStateSuccessReplacesStaleMetadata
        , testProperty "given a ready cache, a failed refresh preserves cached items and records the latest refresh failure" $
            propertyUpdateItemCacheStateFailurePreservesReadyCache
        , testProperty "given no ready cache, a failed refresh leaves the cache unavailable with the new failure reason" $
            propertyUpdateItemCacheStateFailureWithoutReadyCache
        ]
    ]

propertyHandleRequestWithUnlockSuccess :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleRequestWithUnlockSuccess sessionKey items =
  let (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Right sessionKey) (Right items) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState
          == Agent.Unlocked
            sessionKey
            (Agent.CacheReady (Agent.CacheEntry items mockNow) Agent.LatestRefreshSucceeded)
          && response == Agent.Success "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyHandleRequestWithUnlockCacheFillFailure ::
  Agent.SessionKey ->
  Agent.ListItemsError ->
  Property
propertyHandleRequestWithUnlockCacheFillFailure sessionKey listItemsFailure =
  let expectedCacheFailure =
        Agent.cacheFillFailureFromListItemsError sessionKey listItemsFailure
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Right sessionKey) (Left listItemsFailure) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) Agent.Locked)
   in property $
        newState == Agent.Unlocked sessionKey (Agent.CacheFillError expectedCacheFailure)
          && response == Agent.Success "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyDecideGetPasswordLocked :: Agent.LoginItemId -> Property
propertyDecideGetPasswordLocked loginItemId =
  property $
    Agent.decide (Agent.GetPasswordRequest loginItemId) Agent.Locked
      == Agent.Reply (Agent.Failure "locked")

propertyDecideStatusUnlocked :: Agent.SessionKey -> Property
propertyDecideStatusUnlocked sessionKey =
  property $
    Agent.decide Agent.Status (Agent.Unlocked sessionKey Agent.CacheNotYetFilled)
      == Agent.Reply (Agent.Success "unlocked")

propertyDecideListItemsUnlocked :: Agent.SessionKey -> Agent.CacheEntry -> Agent.LatestRefreshStatus -> Property
propertyDecideListItemsUnlocked sessionKey cacheEntry latestRefreshStatus =
  property $
    Agent.decide
      Agent.ListItems
      (Agent.Unlocked sessionKey (Agent.CacheReady cacheEntry latestRefreshStatus))
      == Agent.ListItemsAction cacheEntry

propertyDecideListItemsFailedCacheFill :: Agent.SessionKey -> Agent.CacheFillFailure -> Property
propertyDecideListItemsFailedCacheFill sessionKey cacheFillFailure =
  property $
    Agent.decide
      Agent.ListItems
      (Agent.Unlocked sessionKey (Agent.CacheFillError cacheFillFailure))
      == Agent.Reply (Agent.Failure "item cache unavailable")

propertyDecideGetPasswordUnlocked :: Agent.SessionKey -> Agent.LoginItemId -> Property
propertyDecideGetPasswordUnlocked sessionKey loginItemId =
  property $
    Agent.decide (Agent.GetPasswordRequest loginItemId) (Agent.Unlocked sessionKey Agent.CacheNotYetFilled)
      == Agent.GetPasswordAction sessionKey loginItemId

propertyHandleRequestWithStatusUnlocked :: Agent.SessionKey -> Property
propertyHandleRequestWithStatusUnlocked sessionKey =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.Status currentState)
   in property $
        newState == currentState
          && response == Agent.Success "unlocked"
          && effects == []

propertyHandleRequestWithGetPasswordPreservesState ::
  Agent.AgentState ->
  Agent.LoginItemId ->
  Either Bitwarden.GetPasswordError Agent.PasswordValue ->
  Property
propertyHandleRequestWithGetPasswordPreservesState initialState loginItemId mockGetPasswordResult =
  let (newState, _, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) mockGetPasswordResult)
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) initialState)
   in property (newState == initialState && effects == [])

propertyHandleRequestWithListItemsPreservesState ::
  Agent.SessionKey ->
  Agent.CacheEntry ->
  Agent.LatestRefreshStatus ->
  Property
propertyHandleRequestWithListItemsPreservesState sessionKey cacheEntry latestRefreshStatus =
  let currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady cacheEntry latestRefreshStatus)
      items = Agent.cacheEntryItems cacheEntry
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Left (Agent.ListItemsFailed "list-items should not hit the backend")) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.ItemList items (Agent.cacheAgeSeconds mockNow cacheEntry)
          && effects == []

propertyHandleRequestWithListItemsReportsExactCacheAge ::
  Agent.SessionKey ->
  [Agent.ItemSummary] ->
  Agent.LatestRefreshStatus ->
  Agent.CacheAgeSeconds ->
  Property
propertyHandleRequestWithListItemsReportsExactCacheAge sessionKey items latestRefreshStatus cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
      currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady cacheEntry latestRefreshStatus)
      (_, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Left (Agent.ListItemsFailed "list-items should not hit the backend")) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        response == Agent.ItemList items cacheAgeSecondsValue
          && null effects

propertyHandleRequestWithListItemsNotYetFilled :: Agent.SessionKey -> Property
propertyHandleRequestWithListItemsNotYetFilled sessionKey =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.Failure "item cache unavailable"
          && effects == []

propertyHandleRequestWithListItemsFailedCacheFill ::
  Agent.SessionKey ->
  Agent.CacheFillFailure ->
  Property
propertyHandleRequestWithListItemsFailedCacheFill sessionKey cacheFillFailure =
  let currentState = Agent.Unlocked sessionKey (Agent.CacheFillError cacheFillFailure)
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in property $
        newState == currentState
          && response == Agent.Failure "item cache unavailable"
          && effects == []

propertyHandleRequestWithGetPasswordLocked :: Agent.LoginItemId -> Property
propertyHandleRequestWithGetPasswordLocked loginItemId =
  let currentState = Agent.Locked
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == Agent.Failure "locked"
          && effects == []

propertyHandleRequestWithGetPasswordSuccess :: Agent.SessionKey -> Agent.LoginItemId -> Agent.PasswordValue -> Property
propertyHandleRequestWithGetPasswordSuccess sessionKey loginItemId passwordValue =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Right passwordValue))
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property $
        newState == currentState
          && response == Agent.PasswordResult loginItemId passwordValue
          && effects == []

propertyHandleRequestWithUnlockedIgnoresUnlockResult :: Agent.SessionKey -> MockEnv -> Property
propertyHandleRequestWithUnlockedIgnoresUnlockResult sessionKey mockEnv =
  let currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      (newState, response, effects) =
        runMockBitwarden
          mockEnv
          (Agent.handleRequestWith (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "secret")) currentState)
   in property $
        newState == currentState
          && response == Agent.Success "already unlocked"
          && effects == []

propertyHandleRequestWithUnknownRequest :: Agent.AgentState -> MockEnv -> Property
propertyHandleRequestWithUnknownRequest initialState mockEnv =
  let (newState, response, effects) =
        runMockBitwarden mockEnv (Agent.handleRequestWith Agent.UnknownRequest initialState)
   in property $
        newState == initialState
          && response == Agent.Failure "unknown request"
          && effects == []

propertyHandleRequestWithStatusDoesNotExposeSessionKey :: Agent.AgentState -> Property
propertyHandleRequestWithStatusDoesNotExposeSessionKey initialState =
  let (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.Status initialState)
   in property
        ( newState == initialState
            && statusResponseMatchesState initialState response
            && not (statusResponseLeaksSessionKey initialState response)
            && effects == []
        )

propertyHandleRequestWithListItemsDoesNotExposeSessionKey :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleRequestWithListItemsDoesNotExposeSessionKey sessionKey items =
  let sampleTime = read "2026-05-20 12:00:00 UTC"
      currentState =
        Agent.Unlocked
          sessionKey
          (Agent.CacheReady (Agent.CacheEntry items sampleTime) Agent.LatestRefreshSucceeded)
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Left (Agent.ListItemsFailed "list-items should not hit the backend")) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleRequestWith Agent.ListItems currentState)
   in itemsDoNotContainSessionKey sessionKey items ==>
        property
          ( newState == currentState
              && response == Agent.ItemList items (Agent.cacheAgeSeconds mockNow (Agent.CacheEntry items sampleTime))
              && not (sessionKeyAppearsInEncodedResponse sessionKey response)
              && effects == []
          )

propertyHandleRequestWithGetPasswordFailureDoesNotExposeSessionKey ::
  Agent.LoginItemId ->
  Agent.SessionKey ->
  Property
propertyHandleRequestWithGetPasswordFailureDoesNotExposeSessionKey loginItemId sessionKey =
  let Agent.SessionKey sessionText = sessionKey
      currentState = Agent.Unlocked sessionKey Agent.CacheNotYetFilled
      leakingError = Bitwarden.GetPasswordFailed ("prefix " <> sessionText <> " suffix")
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left leakingError))
          (Agent.handleRequestWith (Agent.GetPasswordRequest loginItemId) currentState)
   in property
        ( newState == currentState
            && isFailure response
            && not (sessionKeyAppearsInEncodedResponse sessionKey response)
            && encodedResponseContains "<redacted>" response
            && effects == []
        )

propertyPasswordResultShowDoesNotExposePassword :: Agent.LoginItemId -> String -> Property
propertyPasswordResultShowDoesNotExposePassword loginItemId passwordText =
  not (null passwordText) ==>
    property
      ( let passwordNeedle =
              T.pack ("pw-needle-" <> passwordText <> "-end")
            response =
              Agent.PasswordResult
                loginItemId
                (Agent.PasswordValue passwordNeedle)
            rendered = T.pack (show response)
         in rendered /= passwordNeedle
              && not (passwordNeedle `T.isInfixOf` rendered)
      )

propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop ::
  Agent.Request ->
  Agent.AgentState ->
  MockEnv ->
  Property
propertyHandleRequestWithOnlyLockedUnlockStartsRefreshLoop request initialState mockEnv =
  let (newState, _, effects) =
        runMockBitwarden mockEnv (Agent.handleRequestWith request initialState)
   in property $
        case effects of
          [] -> True
          [Agent.StartCacheRefreshLoop sessionKey] ->
            case (request, initialState, newState) of
              (Agent.UnlockRequest _ _, Agent.Locked, Agent.Unlocked unlockedSessionKey _) ->
                sessionKey == unlockedSessionKey
              _ ->
                False
          _ -> False

propertyHandleUnlockFailure :: Agent.UnlockError -> Property
propertyHandleUnlockFailure unlockError =
  let (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left unlockError) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Locked
          && response == expectedFailure unlockError
          && not (encodedResponseContains "secret" response)
          && effects == []

propertyHandleListItemsPreservesState ::
  Agent.CacheEntry ->
  Agent.AgentState ->
  Property
propertyHandleListItemsPreservesState cacheEntry initialState =
  let (newState, _, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleListItems cacheEntry initialState)
   in property (newState == initialState && effects == [])

propertyHandleListItemsReportsExactCacheAge ::
  [Agent.ItemSummary] ->
  Agent.AgentState ->
  Agent.CacheAgeSeconds ->
  Property
propertyHandleListItemsReportsExactCacheAge items initialState cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
      (_, response, effects) =
        runMockBitwarden
          (mkMockEnv (Left Agent.UnlockUnavailable) (Right []) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleListItems cacheEntry initialState)
   in property $
        response == Agent.ItemList items cacheAgeSecondsValue
          && null effects

propertyHandleUnlockSuccess :: Agent.SessionKey -> [Agent.ItemSummary] -> Property
propertyHandleUnlockSuccess sessionKey items =
  let (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Right sessionKey) (Right items) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState
          == Agent.Unlocked
            sessionKey
            (Agent.CacheReady (Agent.CacheEntry items mockNow) Agent.LatestRefreshSucceeded)
          && response == Agent.Success "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyHandleUnlockCacheFillFailure ::
  Agent.SessionKey ->
  Agent.ListItemsError ->
  Property
propertyHandleUnlockCacheFillFailure sessionKey listItemsFailure =
  let expectedCacheFailure =
        Agent.cacheFillFailureFromListItemsError sessionKey listItemsFailure
      (newState, response, effects) =
        runMockBitwarden
          (mkMockEnv (Right sessionKey) (Left listItemsFailure) (Left Bitwarden.GetPasswordUnavailable))
          (Agent.handleUnlock (Agent.Username "me@example.com") (Agent.Password "secret"))
   in property $
        newState == Agent.Unlocked sessionKey (Agent.CacheFillError expectedCacheFailure)
          && response == Agent.Success "unlocked"
          && effects == [Agent.StartCacheRefreshLoop sessionKey]

propertyCacheAgeSecondsIsExact :: [Agent.ItemSummary] -> Agent.CacheAgeSeconds -> Property
propertyCacheAgeSecondsIsExact items cacheAgeSecondsValue =
  let cacheEntry = cacheEntryRefreshedSecondsAgo cacheAgeSecondsValue items
   in property $
        Agent.cacheAgeSeconds mockNow cacheEntry
          == cacheAgeSecondsValue

propertyUpdateItemCacheStateSuccessReplacesState ::
  Agent.ItemCacheState ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesState previousState cacheEntry =
  property $
    Agent.updateItemCacheState previousState (Right cacheEntry)
      == Agent.CacheReady cacheEntry Agent.LatestRefreshSucceeded

propertyUpdateItemCacheStateSuccessReplacesStaleMetadata ::
  Agent.CacheEntry ->
  Agent.CacheFillFailure ->
  Agent.CacheEntry ->
  Property
propertyUpdateItemCacheStateSuccessReplacesStaleMetadata oldCacheEntry oldFailure newCacheEntry =
  property $
    Agent.updateItemCacheState
      (Agent.CacheReady oldCacheEntry (Agent.LatestRefreshFailed oldFailure))
      (Right newCacheEntry)
      == Agent.CacheReady newCacheEntry Agent.LatestRefreshSucceeded

propertyUpdateItemCacheStateFailurePreservesReadyCache ::
  Agent.CacheEntry ->
  Agent.LatestRefreshStatus ->
  Agent.CacheFillFailure ->
  Property
propertyUpdateItemCacheStateFailurePreservesReadyCache cacheEntry latestRefreshStatus cacheFillFailure =
  property $
    Agent.updateItemCacheState
      (Agent.CacheReady cacheEntry latestRefreshStatus)
      (Left cacheFillFailure)
      == Agent.CacheReady cacheEntry (Agent.LatestRefreshFailed cacheFillFailure)

propertyUpdateItemCacheStateFailureWithoutReadyCache ::
  Agent.CacheFillFailure ->
  Agent.CacheFillFailure ->
  Property
propertyUpdateItemCacheStateFailureWithoutReadyCache previousFailure newFailure =
  let notYetFilledResult =
        Agent.updateItemCacheState Agent.CacheNotYetFilled (Left newFailure)
      failedCacheFillResult =
        Agent.updateItemCacheState (Agent.CacheFillError previousFailure) (Left newFailure)
   in property $
        notYetFilledResult == Agent.CacheFillError newFailure
          && failedCacheFillResult == Agent.CacheFillError newFailure

expectedFailure :: Agent.UnlockError -> Agent.Response
expectedFailure Agent.UnlockUnavailable = Agent.Failure "bw login failed"
expectedFailure (Agent.UnlockFailed err) = Agent.Failure (Agent.sanitizeUnlockError (Agent.Password "secret") err)

statusResponseMatchesState :: Agent.AgentState -> Agent.Response -> Bool
statusResponseMatchesState Agent.Locked response = response == Agent.Success "locked"
statusResponseMatchesState (Agent.Unlocked _ _) response = response == Agent.Success "unlocked"

statusResponseLeaksSessionKey :: Agent.AgentState -> Agent.Response -> Bool
statusResponseLeaksSessionKey Agent.Locked _ = False
statusResponseLeaksSessionKey (Agent.Unlocked (Agent.SessionKey sessionKey) _) response =
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

itemsDoNotContainSessionKey :: Agent.SessionKey -> [Agent.ItemSummary] -> Bool
itemsDoNotContainSessionKey (Agent.SessionKey sessionText) items =
  not (TE.encodeUtf8 sessionText `BS.isInfixOf` encodedResponse (Agent.ItemList [] (Agent.CacheAgeSeconds 0)))
    && all itemDoesNotContainSessionKey items
  where
    encodedSessionKey = TE.encodeUtf8 sessionText
    itemDoesNotContainSessionKey item =
      not (encodedSessionKey `BS.isInfixOf` LBS.toStrict (Aeson.encode item))

cacheEntryRefreshedSecondsAgo :: Agent.CacheAgeSeconds -> [Agent.ItemSummary] -> Agent.CacheEntry
cacheEntryRefreshedSecondsAgo (Agent.CacheAgeSeconds ageSeconds) items =
  Agent.CacheEntry items (addUTCTime (negate (fromIntegral ageSeconds)) mockNow)

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
