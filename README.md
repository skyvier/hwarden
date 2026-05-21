# hwarden-agent

Tiny Haskell daemon for one job: accept a local Unix socket JSON request, unlock Bitwarden via `bw login ... --raw`, and expose a small local API backed by the in-memory `BW_SESSION`.

## Prerequisites

- `cabal` and a compatible GHC toolchain
- Bitwarden CLI available as `bw`
- `nc` for the manual socket examples below
- `XDG_RUNTIME_DIR` set in the environment

## Run

Optional:

- `HWARDEN_SERVER_URL`
  Defaults to `https://vault.bitwarden.eu`.
- `HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS`
  Defaults to `60`. Must be a positive integer; invalid or non-positive
  values fall back to `60`. Controls the in-memory item list refresh interval
  after unlock.

```sh
cabal run hwarden-agent
```

The daemon creates:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
$XDG_RUNTIME_DIR/hwarden/bitwarden-cli
```

Both directories are created if needed and forced to mode `0700`.

The agent keeps its Bitwarden CLI state in its own isolated profile under
`$XDG_RUNTIME_DIR/hwarden/bitwarden-cli`, so it does not share the user's
default `bw` state. This isolated profile is session-scoped and is not expected
to persist across reboot.

## Protocol

Clients connect to `$XDG_RUNTIME_DIR/hwarden/agent.sock`, send one JSON object,
close the write side of the connection, and then read one JSON response. Closing
the write side is required because the agent reads until EOF before decoding the
request.

| Command | Request fields | Success response | Common failure response |
| --- | --- | --- | --- |
| `unlock` | `cmd`, `email`, `password` | `{"ok":true,"message":"unlocked"}` or `{"ok":true,"message":"already unlocked"}` | `{"ok":false,"error":"..."}` |
| `status` | `cmd` | `{"ok":true,"message":"locked"}` or `{"ok":true,"message":"unlocked"}` | n/a |
| `list-items` | `cmd` | `{"ok":true,"items":[...],"cache_age_seconds":0}` | `{"ok":false,"error":"locked"}` or `{"ok":false,"error":"item cache unavailable"}` |
| `get-password` | `cmd`, `id` | `{"ok":true,"id":"...","password":"..."}` | `{"ok":false,"error":"locked"}` or `{"ok":false,"error":"..."}` |

Bad JSON returns `{"ok":false,"error":"..."}`. Unknown commands return
`{"ok":false,"error":"unknown request"}`.

## Manual testing with nc

Start the agent in one terminal:

```sh
cabal run hwarden-agent
```

Send an unlock request from another terminal:

```sh
printf '{"cmd":"unlock","email":"me@example.com","password":"MY_PASSWORD"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

`-N` is important here: the agent reads until EOF before decoding the request and writing the response.

Expected success:

```json
{"ok":true,"message":"unlocked"}
```

Expected failure:

```json
{"ok":false,"error":"..."}
```

Check the current lock status:

```sh
printf '{"cmd":"status"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

Expected responses:

```json
{"ok":true,"message":"locked"}
```

or

```json
{"ok":true,"message":"unlocked"}
```

List login items after unlocking:

```sh
printf '{"cmd":"list-items"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

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
it in memory once a minute for the active unlocked session. If a background
refresh fails after at least one successful refresh, the agent keeps serving
the last cached item list and `cache_age_seconds` increases to show staleness.

If the agent is still locked:

```json
{"ok":false,"error":"locked"}
```

Fetch the password for a specific login item by id after unlocking:

```sh
printf '{"cmd":"get-password","id":"1"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

Expected success:

```json
{"ok":true,"id":"1","password":"super-secret"}
```

If the agent is still locked:

```json
{"ok":false,"error":"locked"}
```

If Bitwarden cannot return a usable password for that item:

```json
{"ok":false,"error":"..."}
```

Unknown command:

```sh
printf '{"cmd":"nope"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

Expected response:

```json
{"ok":false,"error":"unknown request"}
```
