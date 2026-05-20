# Item Summary Cache Design

## Summary

The agent should keep an in-memory cache of `ItemSummary` values for the
currently unlocked Bitwarden session. After `bw login` succeeds, the agent
should immediately populate that cache before replying to the `unlock`
request, then refresh it in the background once a minute.

The `list-items` socket response remains local-only and cache-backed. Its
success payload gains one additive field, `cache_age_seconds`, so callers can
tell whether the returned item list is fresh or stale.

## Goals

- Make `list-items` fast after a successful unlock by warming the cache before
  the unlock response is sent.
- Keep cache state in process memory only.
- Cache only non-secret `ItemSummary` data and cache metadata.
- Continue serving the last successful cached item list when background
  refreshes fail.
- Keep the change small and centered on existing request handling.

## Non-goals

- This change does not cache passwords, session tokens, raw Bitwarden payloads,
  or any other secrets.
- This change does not persist cache data to disk.
- This change does not alter authentication flow, socket permissions, or
  session lifecycle beyond adding cache state tied to the unlocked session.
- This change does not add new socket commands.
- This change does not make `list-items` fall back to an uncached foreground
  fetch after unlock-time warmup has completed.

## Architecture

### Cache ownership

`Hwarden.Agent` should own the cache. The current request path already keeps
the unlocked `SessionKey` in `AgentState`, and `list-items` is already routed
through that state machine. Extending the unlocked state with cache metadata is
the smallest way to keep refresh logic and response formatting together.

The `Bitwarden` type class should remain unchanged. It already exposes the only
operations needed for this behavior: `unlock`, `listItems`, and
`getPassword`.

### Cached data model

The unlocked state should carry:

- the current `SessionKey`
- the latest successful `[ItemSummary]`
- the timestamp of the latest successful refresh
- whether the most recent background refresh attempt failed

The cache intentionally excludes all secret-bearing data. `ItemSummary`
contains only item id, name, and username, which are already returned by the
public local socket API.

### Unlock-time warmup

The unlock flow becomes:

1. Run `bw login ... --raw`.
2. If login fails, preserve existing unlock failure behavior.
3. If login succeeds, immediately run `bw list items`.
4. If the item fetch succeeds, store the cache and return unlock success.
5. If the item fetch fails, still transition to the unlocked state, but with an
   empty cache and a recorded refresh failure so the background loop can retry.

This keeps the main success path optimized for the first `list-items` request
without reclassifying a temporary list failure as an authentication failure.

### Background refresh loop

After a successful unlock, the agent should start one background loop for the
current unlocked session. The loop should:

1. sleep for 60 seconds
2. run `bw list items` with the current session key
3. on success, replace the cache contents and refresh timestamp
4. on failure, keep the existing cached data unchanged and only record that the
   latest refresh failed
5. repeat while the agent remains unlocked for the same session

The loop should be tied to the unlocked session so an old loop cannot overwrite
cache state after a later unlock. A simple session-scoped generation token or
equivalent guard is sufficient.

### List-items behavior

`list-items` should read only from the in-memory cache for an unlocked session.

- If cached items exist, return them immediately.
- Include `cache_age_seconds` in every successful `list-items` response.
- If the most recent refresh failed, still return the last successful cached
  items; the growing cache age signals staleness to the caller.
- If the current unlocked session has not produced any successful cache entry
  yet, return a failure response rather than triggering an extra foreground
  fetch.

`cache_age_seconds` should be computed from the latest successful refresh
timestamp. A value of `0` means the cache was just refreshed.

## Error handling

### Unlock failures

If `bw login` fails, preserve current behavior, including password redaction in
error messages.

### Warmup failures

If `bw login` succeeds but the immediate cache warmup fails:

- return the existing unlock success response
- keep the session unlocked
- mark the cache as empty and stale
- start the periodic refresh loop so the cache can recover automatically

This avoids conflating authentication success with temporary item-list
availability.

### Background refresh failures

If a periodic refresh fails:

- do not discard the last successful cached item list
- do not downgrade the unlocked session to locked
- preserve the existing password and session-key secrecy guarantees in errors
  and logs

The staleness signal is carried by `cache_age_seconds`, not by changing the
success/failure shape of cached `list-items` responses after at least one
successful refresh.

## Concurrency

The agent already serializes request handling through an `MVar` state cell.
Background refreshes should update the same state cell so cache writes stay
atomic with request-time reads.

The refresh worker must not block unrelated requests except for the brief
critical section needed to swap cache state. The actual `bw list items`
invocation should happen outside the locked state update path, then write back
the result in a short atomic update.

## Protocol impact

The only protocol change is additive on successful `list-items` responses:

```json
{
  "ok": true,
  "items": [
    { "id": "1", "name": "Battle.net", "username": "user@example.com" }
  ],
  "cache_age_seconds": 0
}
```

Existing `unlock`, `status`, `get-password`, and failure response shapes remain
unchanged.

## Testing

Integration coverage should verify:

1. Unlock success performs an immediate item-list warmup before replying.
2. A successful `list-items` response includes `cache_age_seconds`.
3. Cached items are served without needing every subsequent refresh to succeed.
4. After a refresh failure, the last successful cached items are still served
   and `cache_age_seconds` increases.
5. If warmup fails right after a successful unlock, the agent still reports
   unlock success and `list-items` fails until a later refresh succeeds.
6. Locked-state `list-items` and all `get-password` behavior remain unchanged.

Unit tests in `Hwarden.Agent` may be useful for response shaping and
state-transition logic, but the core confidence should come from integration
tests that exercise the fake `bw` process behavior.

## Files likely to change

- `src/Hwarden/Agent.hs`
  - extend unlocked state with cache metadata
  - add unlock-time warmup handling
  - add background refresh orchestration
  - add `cache_age_seconds` to successful `list-items` responses
- `test/Integration.hs`
  - extend fake `bw` behavior so list results can change across refreshes
  - add coverage for warmup success/failure and stale-cache serving
- `README.md`
  - document the additive `cache_age_seconds` field on successful
    `list-items` responses

## Risks and trade-offs

- Waiting for the first cache fill before returning unlock success makes unlock
  slightly slower in the success path, but it gives the caller a strong
  guarantee that `list-items` is fast immediately afterward.
- Keeping stale cached data available improves availability, but the caller must
  interpret `cache_age_seconds` correctly when refreshes are failing.
- Tying the refresh loop to the unlocked session keeps the design small, but it
  requires care to prevent an older worker from updating state after a later
  unlock.
