# hwarden-agent

Tiny Haskell daemon for one job: accept a local Unix socket JSON request, unlock Bitwarden via `bw login ... --raw`, and expose a small local API backed by the in-memory `BW_SESSION`.

## Run

`XDG_RUNTIME_DIR` must be set.

Optional:

- `HWARDEN_SERVER_URL`
  Defaults to `https://vault.bitwarden.eu`.

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
