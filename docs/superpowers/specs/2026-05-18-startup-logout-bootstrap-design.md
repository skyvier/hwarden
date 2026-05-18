# Startup Logout Bootstrap Design

## Summary

The agent should try to clear any stale login state in its isolated Bitwarden CLI profile during startup. The isolated profile bootstrap already runs through `configureServer`; that helper should be extended to run `bw logout` first, log any logout failure, and then continue to `bw config server`.

Startup should continue as long as `bw config server` succeeds. A failed logout is treated as a best-effort cleanup failure, not a fatal startup error.

## Goals

- Keep the agent's isolated Bitwarden CLI profile closer to a predictable logged-out state during startup.
- Avoid expanding the request-time `Bitwarden` effect boundary for a startup-only concern.
- Preserve the existing rule that startup fails only when the isolated CLI bootstrap cannot be configured for normal use.

## Non-goals

- This change does not guarantee logout on abrupt process termination.
- This change does not add a new socket request or expose arbitrary `bw` commands.
- This change does not persist any new session or password data.
- This change does not introduce shutdown-time logout logic.

## Architecture

### Bootstrap ownership

`configureServer` in `Hwarden.Bitwarden.Real` remains the right place for this behavior. It already owns isolated-profile startup configuration, process creation, and logging. The request-time `Bitwarden` type class should stay focused on domain operations used while handling socket requests (`unlock`, `listItems`, `getPassword`).

### Startup sequence

The startup bootstrap sequence becomes:

1. Run `bw logout` in the isolated `BITWARDENCLI_APPDATA_DIR`.
2. If `bw logout` fails, log the failure text and continue.
3. Run `bw config server <url>` in the same isolated environment.
4. If `bw config server` fails, return the existing fatal startup failure.

This makes logout a best-effort reset step and keeps `bw config server` as the authoritative success criterion for startup.

### Process/environment handling

The existing isolated process helpers in `Hwarden.Bitwarden.Real` should be reused so that:

- `bw logout` runs with `BITWARDENCLI_APPDATA_DIR` set.
- No request-time session state is required.
- Environment assembly stays centralized in one module.

No changes are needed to `runAgent` beyond continuing to call the existing bootstrap function.

## Error handling

### Logout failure

If `bw logout` cannot be launched or exits unsuccessfully:

- log an informational message that logout failed
- include the failure text if available
- continue to `bw config server`

There is no need to special-case exit code `1` at this stage. The failure is logged and ignored uniformly.

### Config failure

If `bw config server` fails:

- preserve the current behavior
- startup aborts before the socket accept loop begins

## Logging

Logging should remain in `Hwarden.Bitwarden.Real`.

Expected log points:

- log when running `bw logout`
- if logout fails, log that startup is continuing despite the failure
- log when running `bw config server`

The logs must not expose passwords or session keys. This change does not add any new secret-bearing values to the startup logs.

## Testing

Integration tests should cover the orchestration behavior through the fake `bw` harness:

1. `bw logout` succeeds and `bw config server` runs; startup succeeds.
2. `bw logout` fails and `bw config server` still runs; startup succeeds.
3. `bw config server` fails after the logout attempt; startup fails.

Existing startup bootstrap tests should continue to pass and can be extended rather than replaced.

No new socket protocol coverage is required because the request/response contract is unchanged.

## Files likely to change

- `src/Hwarden/Bitwarden/Real.hs`
  - extend `configureServer`
  - add a small helper for best-effort logout handling if it improves readability
- `test/Integration.hs`
  - extend the fake `bw` behavior model for logout
  - add integration coverage for logout success/failure during startup
- `README.md`
  - no update expected, because the external request/response contract does not change

## Risks and trade-offs

- Best-effort logout does not prove the profile is logged out when startup continues. It only improves startup hygiene before server configuration.
- Treating logout failure as non-fatal keeps startup robust, but may hide some CLI-state problems behind logs.
- Keeping the logic in `configureServer` preserves a tight boundary and avoids cluttering `runAgent` with more startup steps.
