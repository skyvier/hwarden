# Item Summary Cache Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an in-memory `ItemSummary` cache that is warmed during `unlock`, refreshed every 60 seconds for the active unlocked session, and exposed through `list-items` with an additive `cache_age_seconds` field.

**Architecture:** Keep cache ownership in `Hwarden.Agent` by extending unlocked state with non-secret cache metadata and managing a session-key-scoped background refresh loop there. Leave the `Bitwarden` interface unchanged, update response encoding/parsing for the additive field, and drive behavior with existing pure tests plus the fake-`bw` integration harness.

**Tech Stack:** Haskell, Cabal, `MVar`, Aeson, Tasty, Tasty HUnit, QuickCheck

---

### Task 1: Teach the agent state and JSON layer about cached item lists

**Files:**
- Modify: `src/Hwarden/Agent.hs`
- Modify: `test/Main.hs`
- Modify: `test/golden/item-list.json`

- [ ] **Step 1: Write failing encoding and pure state tests**

Add coverage in `test/Main.hs` for the new response payload and cache-backed list behavior before changing implementation.

```haskell
    , testCase "item-list response encoding includes cache age" $
        assertGoldenEncoding
          "test/golden/item-list.json"
          ( Agent.ItemList
              [ Agent.ItemSummary "1" "Battle.net" "joonas_laukka@hotmail.com",
                Agent.ItemSummary "2" "GitHub" "skyvier"
              ]
              0
          )
```

```haskell
        , testCase "given an unlocked state with cached items, a list-items request triggers a cached list action" $
            Agent.decide
              Agent.ListItems
              (Agent.Unlocked (Agent.SessionKey "session-key") cachedItems Nothing)
              @?= Agent.ListItemsAction
                (Agent.SessionKey "session-key")
                cachedItems
          where
            cachedItems =
              Just
                ( Agent.CacheEntry
                    [Agent.ItemSummary "1" "Battle.net" "skyvier"]
                    sampleTime
                )
            sampleTime = read "2026-05-20 12:00:00 UTC"
```

```haskell
        , testCase "given an unlocked state without cached items, a list-items request replies with cache unavailable" $
            Agent.decide
              Agent.ListItems
              (Agent.Unlocked (Agent.SessionKey "session-key") Nothing (Just Agent.RefreshFailed))
              @?= Agent.Reply (Agent.Failure "item cache unavailable")
```

- [ ] **Step 2: Run the focused tests and confirm they fail**

Run:

```bash
HOME=/tmp cabal test --test-options="--pattern=encoding || --pattern=state transitions"
```

Expected: compile or assertion failures because `ItemList` still has the old shape and the unlocked state has no cache metadata yet.

- [ ] **Step 3: Extend `Response`, `AgentState`, and pure decision handling**

Update `src/Hwarden/Agent.hs` so successful item-list responses carry cache age, and unlocked state carries the active session plus optional cached items and refresh health.

```haskell
data Response
  = Success Text
  | ItemList [ItemSummary] Int
  | PasswordResult LoginItemId PasswordValue
  | Failure Text
  deriving (Eq)

data CacheRefreshStatus
  = RefreshHealthy
  | RefreshFailed
  deriving (Eq, Show)

data CacheEntry = CacheEntry
  { cacheEntryItems :: [ItemSummary],
    cacheEntryRefreshedAt :: UTCTime
  }
  deriving (Eq, Show)

data AgentState
  = Locked
  | Unlocked SessionKey (Maybe CacheEntry) (Maybe CacheRefreshStatus)
  deriving (Eq, Show)
```

```haskell
instance ToJSON Response where
  toJSON (ItemList items cacheAgeSecondsValue) =
    object
      [ "ok" .= True,
        "items" .= items,
        "cache_age_seconds" .= cacheAgeSecondsValue
      ]
```

```haskell
instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then
        (PasswordResult . LoginItemId <$> obj .: "id" <*> (PasswordValue <$> obj .: "password"))
          <|> (ItemList <$> obj .: "items" <*> obj .: "cache_age_seconds")
          <|> (Success <$> obj .: "message")
      else Failure <$> obj .: "error"
```

```haskell
decide ListItems agentState =
  case agentState of
    Locked -> Reply (Failure "locked")
    Unlocked sessionKey (Just cachedItemList) _ ->
      ListItemsAction sessionKey cachedItemList
    Unlocked _ Nothing _ ->
      Reply (Failure "item cache unavailable")
```

Keep all password and session-key sanitizing logic untouched.

- [ ] **Step 4: Update the list-items handler to return cached data**

Replace the current request-time `bw list items` call from `handleListItems` with cache formatting logic only.

```haskell
data Decision
  = Unlock Username Password
  | ListItemsAction SessionKey CacheEntry
  | GetPasswordAction SessionKey LoginItemId
  | Reply Response
  deriving (Eq, Show)
```

```haskell
handleListItems :: CacheEntry -> AgentState -> AgentT (AgentState, Response)
handleListItems cacheEntry agentState = do
  currentTime <- liftIO getCurrentTime
  let ageSeconds =
        floor (diffUTCTime currentTime (cacheEntryRefreshedAt cacheEntry))
  pure
    ( agentState,
      ItemList (cacheEntryItems cacheEntry) ageSeconds
    )
```

Do not remove the `Bitwarden.listItems` capability yet; later tasks use it for unlock-time warmup and background refreshes.

- [ ] **Step 5: Update the golden file**

Change `test/golden/item-list.json` to include the additive cache age field.

```json
{"ok":true,"items":[{"id":"1","name":"Battle.net","username":"joonas_laukka@hotmail.com"},{"id":"2","name":"GitHub","username":"skyvier"}],"cache_age_seconds":0}
```

- [ ] **Step 6: Re-run the focused tests**

Run:

```bash
HOME=/tmp cabal test --test-options="--pattern=encoding || --pattern=state transitions"
```

Expected: PASS for encoding and pure state transition tests.

- [ ] **Step 7: Commit the pure cache-shape changes**

```bash
git add src/Hwarden/Agent.hs test/Main.hs test/golden/item-list.json
git commit -m "Add cache-aware item list response" -m "Item-list responses now carry cache_age_seconds and the\nagent state can represent an unlocked session with cached item\nsummaries. The list-items request path becomes cache-backed in the\npure state machine while keeping password and session handling\nunchanged."
```

### Task 2: Warm the cache during unlock and refresh it in the background

**Files:**
- Modify: `src/Hwarden/Agent.hs`
- Modify: `test/Integration.hs`

- [ ] **Step 1: Write failing integration tests for warmup and stale-cache serving**

Extend `test/Integration.hs` with behavior-driven tests before implementing the worker. Add a helper that decodes raw JSON when the strongly typed `Response` shape is not enough for field-level assertions.

```haskell
    , testCase "sending unlock then list-items returns the warmed cache with cache age" $ do
        agent <- setupAgent cachedAgentConfig
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        itemsJson <- sendRawRequest (socketPath agent) "{\"cmd\":\"list-items\"}"
        cleanupAgent agent
        assertEqual "expected successful unlock response" (Agent.Success "unlocked") unlockResponse
        itemsJson @?= "{\"ok\":true,\"items\":[{\"id\":\"1\",\"name\":\"Battle.net\",\"username\":\"joonas\"}],\"cache_age_seconds\":0}"
```

```haskell
    , testCase "sending list-items after a refresh failure still returns the last successful cache" $ do
        agent <- setupAgent rotatingListAgentConfig
        _ <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        threadDelay 1200000
        staleResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        staleResponse @?=
          Agent.ItemList [Agent.ItemSummary "1" "Battle.net" "joonas"] 1
```

```haskell
    , testCase "when the session expires during background refresh, the agent becomes locked" $ do
        agent <- setupAgent expiringSessionAgentConfig
        _ <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        threadDelay 1200000
        statusResponse <- sendRequest (socketPath agent) Agent.Status
        itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual "expected locked status after session expiry" (Agent.Success "locked") statusResponse
        assertEqual "expected locked list-items response after session expiry" (Agent.Failure "locked") itemsResponse
```

```haskell
    , testCase "sending unlock succeeds even when the initial cache warmup fails" $ do
        agent <- setupAgent warmupFailureAgentConfig
        unlockResponse <-
          sendRequest
            (socketPath agent)
            (Agent.UnlockRequest (Agent.Username "me@example.com") (Agent.Password "good-password"))
        itemsResponse <- sendRequest (socketPath agent) Agent.ListItems
        cleanupAgent agent
        assertEqual "expected unlock success despite warmup failure" (Agent.Success "unlocked") unlockResponse
        assertEqual "expected cache-unavailable failure before a successful refresh" (Agent.Failure "item cache unavailable") itemsResponse
```

- [ ] **Step 2: Run the integration slice and confirm it fails**

Run:

```bash
HOME=/tmp cabal test --test-options="--pattern=integration"
```

Expected: failures because unlock still returns without warming a cache, `list-items` still depends on foreground behavior, the agent never re-locks on session expiry, and the fake `bw` harness cannot yet model refresh sequences.

- [ ] **Step 3: Add cache metadata and a session-scoped refresh worker in `Hwarden.Agent`**

Introduce a small cache record that stores the latest successful items with a refresh timestamp, and tie the worker to the current `SessionKey`. This is sufficient because the current protocol does not allow replacing one unlocked session with another through a second successful `unlock`.

```haskell
data CacheEntry = CacheEntry
  { cacheEntryItems :: [ItemSummary],
    cacheEntryRefreshedAt :: UTCTime
  }

data AgentState
  = Locked
  | Unlocked
      { unlockedSessionKey :: SessionKey,
        unlockedCache :: Maybe CacheEntry,
        unlockedRefreshStatus :: Maybe CacheRefreshStatus
      }
```

```haskell
initializeUnlockedState :: SessionKey -> Maybe CacheEntry -> Maybe CacheRefreshStatus -> AgentState
initializeUnlockedState sessionKey cacheEntry refreshStatus =
  Unlocked
    { unlockedSessionKey = sessionKey,
      unlockedCache = cacheEntry,
      unlockedRefreshStatus = refreshStatus
    }
```

Use `Data.Time.Clock` for timestamps and derive `cache_age_seconds` at read time with `diffUTCTime`. Add a small classifier for `ListItemsError` that distinguishes transient failures from session-expired failures based on sanitized `bw list items` error text.

- [ ] **Step 4: Warm the cache synchronously during `unlock`**

Split unlock into login and immediate list refresh. Keep the agent unlocked even if warmup fails.

```haskell
handleUnlock :: Env -> MVar AgentState -> AgentState -> Username -> Password -> AgentT (AgentState, Response)
handleUnlock agentEnv agentStateVar previousState email password = do
  unlockResult <- unlock email password
  case unlockResult of
    Left UnlockUnavailable ->
      pure (Locked, Failure "bw login failed")
    Left (UnlockFailed err) ->
      pure (Locked, Failure (sanitizeUnlockError password err))
    Right sessionKey -> do
      initialCacheResult <- refreshCacheNow sessionKey
      currentTime <- liftIO getCurrentTime
      let newState =
            case initialCacheResult of
              Right items ->
                initializeUnlockedState sessionKey (Just (CacheEntry items currentTime)) (Just RefreshHealthy)
              Left _ ->
                initializeUnlockedState sessionKey Nothing (Just RefreshFailed)
      liftIO (startRefreshLoop agentEnv agentStateVar sessionKey)
      pure (newState, Success "unlocked")
```

To support this, change `handleRequest` and `handleRequestWith` so `handleUnlock` receives the shared `MVar AgentState` and can launch the worker. `previousState` is no longer needed once generation tracking is removed.

- [ ] **Step 5: Implement the minute-based refresh loop**

Run `bw list items` outside the `MVar` critical section, then commit results with a short guarded state update. The worker should exit if the session key no longer matches the current unlocked state, and should also exit after it re-locks the agent for an expired session.

```haskell
startRefreshLoop :: Env -> MVar AgentState -> SessionKey -> IO ()
startRefreshLoop agentEnv agentStateVar sessionKey =
  void . forkIO $ loop
  where
    loop = do
      threadDelay refreshIntervalMicroseconds
      shouldContinue <- refreshOnce
      when shouldContinue loop

    refreshOnce = do
      refreshedAt <- getCurrentTime
      refreshResult <- runAgentT agentEnv (refreshCacheNow sessionKey)
      modifyMVar agentStateVar $ \agentState ->
        pure (applyRefreshResult sessionKey refreshedAt refreshResult agentState)

refreshIntervalMicroseconds :: Int
refreshIntervalMicroseconds = 60 * 1000000
```

```haskell
applyRefreshResult :: SessionKey -> UTCTime -> Either ListItemsError [ItemSummary] -> AgentState -> (AgentState, Bool)
applyRefreshResult sessionKey refreshedAt refreshResult agentState =
  case agentState of
    Unlocked currentSessionKey cacheEntry _
      | currentSessionKey == sessionKey ->
          case refreshResult of
            Right items ->
              ( Unlocked currentSessionKey (Just (CacheEntry items refreshedAt)) (Just RefreshHealthy),
                True
              )
            Left err
              | isExpiredSessionError err -> (Locked, False)
              | otherwise ->
                  ( Unlocked currentSessionKey cacheEntry (Just RefreshFailed),
                    True
                  )
    _ -> (agentState, False)
```

Keep error sanitization for `listItems` failures if they are logged, but do not expose session-bearing details in responses.

- [ ] **Step 6: Extend the fake `bw` integration harness for refresh sequences**

Replace the single `listItemsBehavior` field with a list or `IORef`-backed sequence so tests can model warmup success followed by later failure.

```haskell
data BwBehavior = BwBehavior
  { logoutBehavior :: CommandBehavior,
    configServerBehavior :: CommandBehavior,
    unlockBehavior :: CommandBehavior,
    listItemsBehaviors :: [CommandBehavior],
    getPasswordBehavior :: CommandBehavior
  }
```

```haskell
scriptFor :: FilePath -> String -> BwBehavior -> BS8.ByteString
scriptFor expectedAppDataDir expectedServerUrl bwBehavior =
  BS8.unlines
    [ "#!/bin/sh",
      "LIST_STATE_FILE=\"$BITWARDENCLI_APPDATA_DIR/list-items-count\"",
      "count=0",
      "if [ -f \"$LIST_STATE_FILE\" ]; then count=$(cat \"$LIST_STATE_FILE\"); fi",
      "case \"$1\" in",
      "  list)",
      "    printf '%s' $((count + 1)) > \"$LIST_STATE_FILE\"",
      emitIndexedListBehavior "      " (listItemsBehaviors bwBehavior),
      "    ;;"
    ]
```

Use deterministic payloads so the first `list` call warms the cache with one item and later calls can either fail transiently or fail with a recognizable session-expired message such as `"Not logged in."`.

- [ ] **Step 7: Re-run the integration slice**

Run:

```bash
HOME=/tmp cabal test --test-options="--pattern=integration"
```

Expected: PASS for warmup success, stale-cache serving after refresh failure, session-key-mismatch worker shutdown, warmup-failure unlock success, locked behavior, and unchanged password flow.

- [ ] **Step 8: Commit the runtime cache behavior**

```bash
git add src/Hwarden/Agent.hs test/Integration.hs
git commit -m "Add in-memory item cache refresh" -m "Unlock now warms an in-memory item summary cache before\nreplying and starts a one-minute background refresh loop for the\nactive unlocked session. List-items serves cached data with stale\ncache retention when refreshes fail, while password and session\nsecrecy rules remain unchanged."
```

### Task 3: Document the additive response contract and run full verification

**Files:**
- Modify: `README.md`

- [ ] **Step 1: Update the README examples**

Document the additive `cache_age_seconds` field on successful `list-items` responses and note that the result is served from an in-memory cache refreshed once a minute after unlock.

```md
Expected success:

```json
{
  "ok": true,
  "items": [
    { "id": "1", "name": "Battle.net", "username": "joonas_laukka@hotmail.com" }
  ],
  "cache_age_seconds": 0
}
```

The agent warms this cache during a successful `unlock` request and refreshes
it in memory once a minute for the active unlocked session.
```

- [ ] **Step 2: Run the full test suite**

Run:

```bash
HOME=/tmp cabal test
```

Expected: PASS for parsing, encoding, filesystem, state transitions, and integration.

- [ ] **Step 3: Run a build-only sanity check**

Run:

```bash
HOME=/tmp cabal build
```

Expected: successful build with no compile errors.

- [ ] **Step 4: Commit the docs update and verification**

```bash
git add README.md
git commit -m "Document cached item list responses" -m "The README now describes the cache-backed list-items response\nand the additive cache_age_seconds field. This keeps the local\nsocket contract aligned with the new in-memory refresh behavior."
```
