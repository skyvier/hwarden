# hwarden-agent

Tiny Haskell daemon for one job: accept a local Unix socket JSON request to unlock Bitwarden via `bw login ... --raw`, then keep the resulting `BW_SESSION` only in process memory.

## Run

`XDG_RUNTIME_DIR` must be set.

```sh
cabal run hwarden-agent
```

The daemon creates:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
```

The directory is created if needed and forced to mode `0700`.

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

Unknown command:

```sh
printf '{"cmd":"nope"}' \
  | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

Expected response:

```json
{"ok":false,"error":"unknown command"}
```
