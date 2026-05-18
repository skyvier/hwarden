# Startup Logout Bootstrap Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make startup attempt `bw logout` in the isolated Bitwarden CLI profile before `bw config server`, treating logout failure as non-fatal while keeping config failure fatal.

**Architecture:** Keep all startup bootstrap behavior inside `Hwarden.Bitwarden.Real.configureServer`. Add a best-effort logout helper that reuses the existing isolated process helpers and logs failures before continuing to server configuration. Extend the fake `bw` integration harness so startup tests cover logout success, logout failure, and config failure after logout.

**Tech Stack:** Haskell, Cabal, `katip`, `process`, `tasty`, Unix socket integration tests

---

### Task 1: Add Integration Coverage For Startup Logout Behavior

**Files:**
- Modify: `test/Integration.hs`
- Test: `test/Integration.hs`

- [ ] **Step 1: Write the failing integration tests**

Add these test cases to `integrationTests` in `test/Integration.hs` near the existing startup bootstrap tests:

```haskell
    , testCase "agent startup continues when bw logout fails before server configuration" $ do
        agent <-
          setupAgent
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandFails "logout failed"
                    }
              }
        cleanupAgent agent
    , testCase "agent startup fails when bw config server fails after the logout attempt" $ do
        agent <-
          spawnConfiguredAgent
            defaultAgentConfig
              { agentBwBehavior =
                  defaultFailingBw
                    { logoutBehavior = CommandSucceeds "",
                      configServerBehavior = CommandFails "config failed"
                    }
              }
        exitedBeforeSocketReady <- waitForProcessExitBeforeSocketReady agent
        exitCode <- waitForProcess (processHandle agent)
        removeDirectoryRecursive (tempRoot agent)
        assertBool "expected startup failure before socket became ready" exitedBeforeSocketReady
        assertBool "expected startup failure exit code" (exitCode /= ExitSuccess)
```

Also extend the startup comment block so it still accurately describes that startup now runs logout and then config through the fake `bw` script.

- [ ] **Step 2: Extend the fake `bw` behavior model with logout support**

Update the support types and defaults in `test/Integration.hs` so the fake harness can model `bw logout` independently:

```haskell
data BwBehavior = BwBehavior
  { logoutBehavior :: CommandBehavior,
    configServerBehavior :: CommandBehavior,
    unlockBehavior :: CommandBehavior,
    listItemsBehavior :: CommandBehavior,
    getPasswordBehavior :: CommandBehavior
  }

defaultFailingBw :: BwBehavior
defaultFailingBw =
  BwBehavior
    { logoutBehavior = CommandSucceeds "",
      configServerBehavior = CommandSucceeds "",
      unlockBehavior = CommandFails "credentials were incorrect",
      listItemsBehavior = CommandFails "bw list items failed",
      getPasswordBehavior = CommandFails "bw get password failed"
    }
```

Adjust any explicit `BwBehavior` record literals in the file so they include `logoutBehavior = CommandSucceeds ""`.

- [ ] **Step 3: Teach the fake `bw` script to require logout before config-sensitive commands**

Update `scriptFor` and helpers in `test/Integration.hs` so `bw logout` is a first-class command:

```haskell
      "  logout)",
      emitLogoutBehavior "    " (logoutBehavior bwBehavior),
      "    ;;",
      "  config)",
      "    if [ ! -f \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\" ]; then",
      "      printf '%s\\n' 'logout was not attempted before config' 1>&2",
      "      exit 1",
      "    fi",
      "    if [ \"$2\" = \"server\" ] && [ \"$3\" = \"" <> BS8.pack expectedServerUrl <> "\" ]; then",
      emitConfigBehavior "      " (configServerBehavior bwBehavior),
```

Add a helper like this so both success and failure mark that logout was attempted:

```haskell
emitLogoutBehavior :: BS8.ByteString -> CommandBehavior -> BS8.ByteString
emitLogoutBehavior indent commandBehavior =
  case commandBehavior of
    CommandSucceeds _ ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\"",
          indent <> "exit 0"
        ]
    CommandFails errMessage ->
      BS8.unlines
        [ indent <> ": > \"$BITWARDENCLI_APPDATA_DIR/logout-attempted\"",
          indent <> "while IFS= read -r line; do printf '%s\\n' \"$line\" 1>&2; done <<'EOF'",
          errMessage,
          "EOF",
          indent <> "exit 1"
        ]
```

This keeps the harness aligned with the approved bootstrap rule: logout is attempted first, but startup may continue after logout failure.

- [ ] **Step 4: Run the integration-focused test suite to verify failure**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-show-details=direct'
```

Expected before implementation:
- the new logout-related startup test should fail because production code does not call `bw logout` yet
- existing tests may still pass

- [ ] **Step 5: Commit the failing-test checkpoint**

```bash
git add test/Integration.hs
git commit -m "Add startup logout integration coverage"
```

### Task 2: Implement Best-Effort Startup Logout In The Real Backend

**Files:**
- Modify: `src/Hwarden/Bitwarden/Real.hs`
- Test: `test/Integration.hs`

- [ ] **Step 1: Add a logout helper that reuses the isolated process path**

In `src/Hwarden/Bitwarden/Real.hs`, add a helper near `configureServer` that runs `bw logout`, logs failures, and never returns a fatal error:

```haskell
bestEffortLogout ::
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m ()
bestEffortLogout = do
  logInfo "running bw logout"
  command <- isolatedBwProcess ["logout"]
  result <-
    handleCheckedCommand
      (runCommand command)
      "bw logout failed"
      (const (Right ()))
      sanitizeLogoutFailure
  case result of
    Left err -> logInfo ("bw logout failed; continuing startup: " <> err)
    Right () -> pure ()
```

Add a dedicated sanitizer so empty stderr still produces a readable log message:

```haskell
sanitizeLogoutFailure :: String -> Text
sanitizeLogoutFailure stderrText =
  let trimmed = T.strip (T.pack stderrText)
   in if T.null trimmed then "bw logout failed" else trimmed
```

- [ ] **Step 2: Update `configureServer` to call logout first**

Change `configureServer` in `src/Hwarden/Bitwarden/Real.hs` to:

```haskell
configureServer ::
  (KatipContext m, MonadIO m, MonadReader r m, HasBitwardenCliConfig r) =>
  m (Either Text ())
configureServer = do
  bestEffortLogout
  serverUrl <- asks bitwardenServerUrl
  logInfo "running bw config server"
  command <- isolatedBwProcess ["config", "server", T.unpack serverUrl]
  handleCheckedCommand
    (runCommand command)
    "bw config server failed"
    (const (Right ()))
    sanitizeCommandFailure
```

Do not change the `Bitwarden` type class, `runAgent`, or request-handling logic.

- [ ] **Step 3: Run the full test suite to verify the implementation**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test'
```

Expected:
- all existing tests pass
- the new startup logout integration coverage passes

- [ ] **Step 4: Run a build check**

Run:

```bash
HOME=/tmp cabal build
```

Expected:
- successful library, executable, and test-suite build

- [ ] **Step 5: Commit the implementation**

```bash
git add src/Hwarden/Bitwarden/Real.hs test/Integration.hs
git commit -m "Reset isolated bw state during startup"
```

### Task 3: Final Verification And Review

**Files:**
- Modify: none expected
- Test: `src/Hwarden/Bitwarden/Real.hs`, `test/Integration.hs`

- [ ] **Step 1: Inspect the final diff for scope**

Run:

```bash
git diff --stat HEAD~2..HEAD
git diff -- src/Hwarden/Bitwarden/Real.hs test/Integration.hs
```

Expected:
- only the real backend startup bootstrap and integration harness changed
- no request/response contract changes

- [ ] **Step 2: Re-run the targeted bootstrap regression tests**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test --test-options="--pattern=integration"'
```

Expected:
- startup logout success/failure coverage passes
- existing socket integration tests remain green

- [ ] **Step 3: Prepare the branch for review**

Run:

```bash
git status --short
```

Expected:
- clean worktree before requesting review or creating a PR update

