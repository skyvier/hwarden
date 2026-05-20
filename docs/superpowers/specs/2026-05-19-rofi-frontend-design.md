# Rofi Frontend Design

## Summary

Add a minimalist local frontend for `hwarden-agent` built from shell scripts and
`rofi`. The frontend should let the user unlock the agent when needed, browse
login items, fetch a password for a selected item, and copy that password to
the X11 clipboard. It should also provide a few small shell helpers so the same
socket API can be reused from scripts without changing the backend.

This work must remain strictly frontend-only. The backend request protocol,
agent implementation, and Cabal package are out of scope.

## Goals

- Provide a daily-usable rofi workflow for unlocking and retrieving passwords.
- Keep the prototype minimal: shell scripts only, no new Haskell frontend.
- Reuse the existing local Unix socket API exactly as it exists today.
- Add low-level and high-level shell helpers for scripting and composition.
- Avoid persisting passwords, OTPs, or session secrets anywhere on disk.

## Non-goals

- No backend changes.
- No new socket requests or response formats.
- No Wayland clipboard support in this MVP.
- No long-lived shell session integration that exports secrets globally.
- No frontend caching of item lists or passwords.

## Architecture

### Script layout

The frontend should live entirely under `scripts/`.

Proposed files:

- `scripts/hwarden-lib`
  Shared shell helpers for socket IO, JSON parsing, error handling, and rofi
  prompt wrappers.
- `scripts/hwarden-status`
  Low-level wrapper that prints the agent status.
- `scripts/hwarden-unlock`
  Low-level wrapper that performs unlock with required positional arguments and
  an optional email OTP argument.
- `scripts/hwarden-list-items`
  Low-level wrapper that prints the `list-items` JSON response.
- `scripts/hwarden-get-password`
  Low-level wrapper that fetches a password by item id.
- `scripts/hwarden-ensure-unlocked`
  High-level helper that checks `status`, prompts for credentials through rofi
  if needed, and retries once with email OTP when the agent reports that the
  code is required.
- `scripts/hwarden-copy-password`
  High-level helper that ensures the agent is unlocked, fetches a password by
  item id, and copies it to the X11 clipboard.
- `scripts/hwarden-rofi`
  Main UI entrypoint. It ensures unlock, lists login items in rofi, lets the
  user choose one, and copies the resulting password to the clipboard.

### Communication with the agent

All frontend scripts should communicate with the backend over
`$XDG_RUNTIME_DIR/hwarden/agent.sock` using `nc -N -U`.

The shared library should centralize:

- socket path derivation
- request encoding
- request/response transport
- basic response validation

The frontend should treat the agent as the single source of truth. It should
not attempt to mirror agent state locally.

### Unlock workflow

The unlock flow should be:

1. Ask the agent for `status`.
2. If already unlocked, continue.
3. Otherwise prompt for:
   - email
   - password
4. Send `unlock`.
5. If the agent responds with `"two-factor code required"`, prompt once for an
   email OTP and retry `unlock` with `twoFactorCode`.
6. If the second attempt fails, show the backend error and stop.

The frontend does not need to know anything about Bitwarden methods beyond the
documented frontend limitation that only email OTP is supported.

### Item selection workflow

After the agent is unlocked:

1. Call `list-items`.
2. Render each item as a single rofi line using `name` and `username`.
3. Keep the item `id` as hidden data in the script pipeline.
4. On selection, call `get-password` with the chosen id.
5. Copy the returned password to the X11 clipboard using `xclip`.
6. Exit successfully without printing the password to the terminal.

### Shell helper surface

The low-level helpers should stay close to the raw API:

- `hwarden-status`
- `hwarden-unlock EMAIL PASSWORD [EMAIL_OTP]`
- `hwarden-list-items`
- `hwarden-get-password ITEM_ID`

The high-level helpers should be convenience wrappers, not a second API:

- `hwarden-ensure-unlocked`
- `hwarden-copy-password ITEM_ID`

The helpers should return nonzero exit codes on failure and print concise error
messages to stderr. They may print JSON or plaintext on stdout depending on the
specific command, but they must not print secrets unless the command is
explicitly a secret-fetching primitive.

## Dependencies

The prototype assumes these external tools are present:

- `rofi`
- `jq`
- `nc` with Unix socket support
- `xclip`

The scripts should check for required tools up front and fail clearly when a
dependency is missing.

## Error handling

Frontend errors should be simple and local:

- missing socket or unavailable agent: fail with a concise message
- malformed backend response: fail with a concise message
- locked state after failed unlock: show backend error and stop
- missing clipboard tool: fail with a concise message
- empty rofi selection or user cancellation: exit quietly with a nonzero code

The scripts should not use `set -x` or any debug mode that would echo secrets.

## Security

- Passwords and OTPs must never be written to temp files.
- Passwords should only flow through shell variables and pipes long enough to
  be sent to the agent or copied to the clipboard.
- The main rofi entrypoint should not print passwords to stdout.
- Helper commands that do print secrets, such as `hwarden-get-password`, should
  be documented as low-level primitives intended for explicit scripting use.
- No helper should export secrets into the user’s environment automatically.

## Testing

This frontend work is shell-heavy, so testing should focus on lightweight
verification:

- syntax-check all added scripts with `bash -n`
- run the existing backend test suite to confirm no accidental backend breakage
- manually review script command construction for secret-safe behavior

No backend tests need to change for this frontend prototype.

## Files likely to change

- add `scripts/`
- update `README.md`
- add frontend spec/plan docs under `docs/superpowers/`

## Risks and trade-offs

- Shell scripting keeps the prototype small, but JSON handling must stay
  disciplined to avoid brittle parsing.
- Low-level helpers that expose secrets on stdout are useful for scripts, but
  they must stay clearly separated from the user-facing rofi flow.
- X11-only clipboard support keeps the MVP simple, but it will not cover
  Wayland users yet.
