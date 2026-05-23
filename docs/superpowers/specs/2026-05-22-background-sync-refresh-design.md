# Background Sync Refresh Design

## Summary

The agent should run `bw sync` before `bw list items` during background cache
refreshes. If `bw sync` fails, the refresh attempt should fail as a whole, the
agent should keep serving the last successful cached item list, and the latest
refresh status should record the failure.

Unlock-time cache warmup should remain unchanged. It should continue to use
`bw list items` directly after a successful `bw login`.

## Goals

- Refresh cached item summaries from the latest remote Bitwarden state before
  replacing the in-memory cache.
- Keep the stale-cache fallback behavior unchanged when a background refresh
  step fails.
- Keep the change small and aligned with the existing `Bitwarden` abstraction.

## Non-goals

- This change does not alter the unlock flow.
- This change does not add new socket commands or response fields.
- This change does not persist any cache or session data.
- This change does not make `bw sync` part of `get-password` or foreground
  `list-items` handling.

## Architecture

### Bitwarden interface

`Hwarden.Bitwarden` should gain:

- a `SyncError` type with unavailable and command-failed variants
- a `sync :: SessionKey -> m (Either SyncError ())` method on the
  `Bitwarden` type class

This keeps `Hwarden.Agent` independent of the concrete CLI runner while making
the new refresh precondition explicit in tests and production code.

### Real CLI implementation

`Hwarden.Bitwarden.Real` should implement `sync` by running:

```sh
bw sync
```

with the authenticated `BW_SESSION` environment used by the existing session
commands. Error handling should match the current pattern:

- map process launch failures to `SyncUnavailable`
- map non-zero exits to `SyncFailed Text`
- sanitize any echoed session key from stderr before storing or returning the
  message

### Background refresh flow

The background refresh path should become:

1. run `bw sync`
2. if sync fails, stop and treat the refresh as failed
3. if sync succeeds, run `bw list items`
4. if listing succeeds, replace the cache entry and mark refresh success
5. if listing fails, preserve the existing stale-cache behavior

This sequencing applies only to the periodic refresh loop. The existing
unlock-time cache warmup should continue calling `bw list items` directly.

## Error handling

If `bw sync` fails during background refresh:

- preserve the last successful cached item list when one exists
- mark the latest refresh status as failed
- continue the refresh loop on the next interval

`SyncFailed` messages should be sanitized the same way `list-items` and
`get-password` failures are sanitized today so the in-memory session key never
appears in logs or responses.

## Testing

New tests should prove behavior that existing cache-refresh tests do not:

1. A background refresh runs `bw sync` before attempting to replace the cache.
2. A `bw sync` failure causes the refresh attempt to fail even if a later
   `bw list items` result would have succeeded.
3. After a background `bw sync` failure, the agent still serves stale cached
   items with a growing `cache_age_seconds`.

Unit coverage in `test/Main.hs` should focus on refresh result mapping.
Integration coverage in `test/Integration.hs` should extend the fake `bw`
script behavior so sync outcomes can vary independently from list-items
outcomes.

## Files likely to change

- `src/Hwarden/Bitwarden.hs`
- `src/Hwarden/Bitwarden/Real.hs`
- `src/Hwarden/Agent.hs`
- `test/Main.hs`
- `test/Integration.hs`
