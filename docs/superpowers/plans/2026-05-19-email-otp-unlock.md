# Email OTP Unlock Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add optional email OTP support to `unlock` and make OTP-required login fail instead of hanging when no `twoFactorCode` is supplied.

**Architecture:** Extend the unlock request with an optional secret-bearing `TwoFactorCode`, thread it through the `Bitwarden` effect boundary, and update the real `bw login` path to support `--method 1 --code <code>` for email OTP. Replace the current unlock subprocess path with an explicitly non-interactive, timeout-bounded implementation so missing OTP results in a normal failure instead of indefinite blocking.

**Tech Stack:** Haskell, Cabal, `aeson`, `process`, `katip`, `tasty`, `QuickCheck`, Unix socket integration tests

---

### Task 1: Add Pure Unlock Request Coverage And Secret Type

**Files:**
- Modify: `src/Hwarden/Types.hs`
- Modify: `src/Hwarden/Agent.hs`
- Modify: `test/Main.hs`
- Test: `test/Main.hs`

- [ ] **Step 1: Write the failing pure tests for `twoFactorCode` parsing, encoding, and redaction**

In `test/Main.hs`, add pure coverage near the existing unlock parsing/encoding tests:

```haskell
    , testCase "request parser decodes unlock payload with twoFactorCode" $ do
        let payload =
              BS8.pack
                "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\",\"twoFactorCode\":\"249213\"}"
        Aeson.eitherDecodeStrict' payload
          @?= Right
            ( Agent.UnlockRequest
                (Agent.Username "me@example.com")
                (Agent.Password "bad-password")
                (Just (Agent.TwoFactorCode "249213"))
            )
```

Add one golden or example encoding test:

```haskell
    , testCase "unlock request encoding includes twoFactorCode when present" $
        Aeson.encode
          ( Agent.UnlockRequest
              (Agent.Username "me@example.com")
              (Agent.Password "bad-password")
              (Just (Agent.TwoFactorCode "249213"))
          )
          @?= "{\"cmd\":\"unlock\",\"email\":\"me@example.com\",\"password\":\"bad-password\",\"twoFactorCode\":\"249213\"}"
```

Add a property test that `show` on an unlock request never exposes the OTP:

```haskell
propertyUnlockRequestShowDoesNotExposeTwoFactorCode ::
  Agent.Username ->
  Agent.Password ->
  String ->
  Property
propertyUnlockRequestShowDoesNotExposeTwoFactorCode username password rawCode =
  let code = Agent.TwoFactorCode (T.pack rawCode)
      request = Agent.UnlockRequest username password (Just code)
   in property $
        not (T.pack rawCode `T.isInfixOf` T.pack (show request))
```

- [ ] **Step 2: Add `TwoFactorCode` in `src/Hwarden/Types.hs`**

Add a new secret-bearing type next to `Password` and `PasswordValue`:

```haskell
newtype TwoFactorCode = TwoFactorCode Text
  deriving (Eq)

instance Show TwoFactorCode where
  show _ = "[REDACTED]"
```

Export it from `Hwarden.Types`.

- [ ] **Step 3: Extend `UnlockRequest` to carry `Maybe TwoFactorCode`**

Update `src/Hwarden/Agent.hs` so:

```haskell
data Request
  = UnlockRequest Username Password (Maybe TwoFactorCode)
  | Status
  | ListItems
  | GetPasswordRequest LoginItemId
  | UnknownRequest
```

Update JSON parsing/encoding:

```haskell
"unlock" ->
  UnlockRequest
    <$> (Username <$> obj .: "email")
    <*> (Password <$> obj .: "password")
    <*> (fmap TwoFactorCode <$> obj .:? "twoFactorCode")
```

and

```haskell
toJSON (UnlockRequest (Username email) (Password password) maybeCode) =
  object $
    [ "cmd" .= ("unlock" :: Text)
    , "email" .= email
    , "password" .= password
    ]
    <> maybe [] (\(TwoFactorCode code) -> ["twoFactorCode" .= code]) maybeCode
```

Also update any pattern matches on `UnlockRequest` in `decide`, `handleRequestWith`, and tests.

- [ ] **Step 4: Run the pure test suite to verify the new tests fail for the right reason**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-options="--pattern=parsing || --pattern=encoding || --pattern=state transitions"'
```

Expected before implementation is complete:
- compile or test failures pointing to missing `TwoFactorCode` / outdated unlock request arity

- [ ] **Step 5: Implement the minimal code until the pure tests pass**

Finish the `TwoFactorCode` plumbing in:
- `src/Hwarden/Types.hs`
- `src/Hwarden/Agent.hs`
- any compile-fixing call sites in `test/Main.hs`

Do not touch the real `Bitwarden` backend yet.

- [ ] **Step 6: Re-run the pure test subset**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-options="--pattern=parsing || --pattern=encoding || --pattern=state transitions"'
```

Expected:
- the new pure request/encoding/redaction coverage passes

- [ ] **Step 7: Commit the pure request/type changes**

```bash
git add src/Hwarden/Types.hs src/Hwarden/Agent.hs test/Main.hs
git commit -m "Add optional unlock two-factor code"
```

### Task 2: Extend The Bitwarden Boundary And Mock Interpreter

**Files:**
- Modify: `src/Hwarden/Bitwarden.hs`
- Modify: `test/Main.hs`
- Test: `test/Main.hs`

- [ ] **Step 1: Change the `Bitwarden.unlock` signature**

In `src/Hwarden/Bitwarden.hs`, update:

```haskell
unlock :: Username -> Password -> Maybe TwoFactorCode -> m (Either UnlockError SessionKey)
```

Import `TwoFactorCode` from `Hwarden.Types`.

- [ ] **Step 2: Update the mock interpreter and mock environment**

In `test/Main.hs`, keep the mock result shape simple, but update the instance to match the new arity:

```haskell
instance Bitwarden.Bitwarden MockBitwarden where
  unlock _ _ _ = MockBitwarden unlockResult
  listItems _ = MockBitwarden listItemsResult
  getPassword _ _ = MockBitwarden getPasswordResult
```

Update all pure-state tests so they construct:

```haskell
Agent.UnlockRequest (Agent.Username "...") (Agent.Password "...") Nothing
```

or with `Just (Agent.TwoFactorCode "...")` where relevant.

- [ ] **Step 3: Add a property that unlock decisions preserve the provided optional code**

Add a pure property in `test/Main.hs`:

```haskell
propertyDecideUnlockPreservesTwoFactorCode ::
  Agent.Username ->
  Agent.Password ->
  Maybe Agent.TwoFactorCode ->
  Property
propertyDecideUnlockPreservesTwoFactorCode username password maybeCode =
  property $
    Agent.decide (Agent.UnlockRequest username password maybeCode) Agent.Locked
      == Agent.Unlock username password maybeCode
```

This requires updating the `Decision` shape in `src/Hwarden/Agent.hs` to carry the optional code too.

- [ ] **Step 4: Run the pure/state test subset**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-options="--pattern=state transitions"'
```

Expected:
- state-machine and mock-interpreter tests pass with the new unlock arity

- [ ] **Step 5: Commit the effect-boundary update**

```bash
git add src/Hwarden/Bitwarden.hs src/Hwarden/Agent.hs test/Main.hs
git commit -m "Thread optional OTP through unlock"
```

### Task 3: Implement Real Email OTP Login And Non-Interactive Failure

**Files:**
- Modify: `src/Hwarden/Bitwarden/Real.hs`
- Test: `src/Hwarden/Bitwarden/Real.hs`

- [ ] **Step 1: Write the failing integration scenarios in the fake `bw` harness**

In `test/Integration.hs`, extend `CommandBehavior`/`BwBehavior` or a small adjacent helper so the fake `bw login` path can distinguish:
- plain successful unlock
- successful unlock with OTP
- OTP required but not supplied
- invalid OTP

Add integration cases such as:

```haskell
    , testCase "sending unlock with twoFactorCode succeeds when bw requires email OTP" $ do
        ...
    , testCase "sending unlock without twoFactorCode fails instead of hanging when bw requires email OTP" $ do
        ...
    , testCase "sending unlock with an invalid twoFactorCode fails normally" $ do
        ...
```

The missing-OTP case should assert a concrete failure response like:

```haskell
Agent.Failure "two-factor code required"
```

- [ ] **Step 2: Teach the fake `bw` script to model email OTP login**

Update `scriptFor` in `test/Integration.hs` so the `login)` case can inspect arguments and simulate:
- `--method 1 --code <code>` success
- missing OTP failure
- invalid OTP failure

Keep the harness pragmatic: it only needs to support the email OTP path, not arbitrary methods.

- [ ] **Step 3: Run the integration suite to verify the new login scenarios fail before backend changes**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-options="--pattern=integration"'
```

Expected before backend implementation:
- the new OTP-related integration tests fail

- [ ] **Step 4: Replace the real unlock path with an explicit non-interactive implementation**

In `src/Hwarden/Bitwarden/Real.hs`, stop using the generic `readCreateProcessWithExitCode` login path for unlock. Introduce a dedicated helper for login, for example:

```haskell
runLoginCommand :: CreateProcess -> IO (Either UnlockError (ExitCode, String, String))
```

or an equivalent shape that:
- uses `createProcess`
- does not allow interactive stdin
- captures stdout/stderr
- waits with a timeout

Then update `unlock` so:
- with `Just (TwoFactorCode code)`, it runs:
  `bw login <email> <password> --raw --method 1 --code <code>`
- with `Nothing`, it runs plain login but maps interactive wait / OTP-required behavior to:
  `Left (UnlockFailed "two-factor code required")`

Keep all logging secret-safe.

- [ ] **Step 5: Run the full test suite**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test'
```

Expected:
- all pure and integration tests pass

- [ ] **Step 6: Run a build check**

Run:

```bash
HOME=/tmp cabal build
```

Expected:
- successful library, executable, and test-suite build

- [ ] **Step 7: Commit the real unlock implementation**

```bash
git add src/Hwarden/Bitwarden/Real.hs test/Integration.hs
git commit -m "Support email OTP during unlock"
```

### Task 4: Document The Email-Only OTP Contract

**Files:**
- Modify: `README.md`
- Test: `README.md`

- [ ] **Step 1: Update the unlock request documentation**

Add an example unlock request with `twoFactorCode`:

```json
{"cmd":"unlock","email":"john@example.com","password":"secret","twoFactorCode":"249213"}
```

Document clearly that:
- `twoFactorCode` is optional
- only Bitwarden email OTP is supported for now
- the method is hardcoded internally

- [ ] **Step 2: Document the missing-OTP behavior**

Add a short note that if Bitwarden requires email OTP and `twoFactorCode` is omitted, unlock fails instead of hanging.

- [ ] **Step 3: Run a final verification pass**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test'
git diff --stat HEAD~4..HEAD
```

Expected:
- tests still pass
- the diff shows only the intended request/backend/test/doc changes

- [ ] **Step 4: Commit the documentation update**

```bash
git add README.md
git commit -m "Document email OTP unlock support"
```

