# hwarden-agent

Tiny Haskell daemon for one job: accept a local Unix socket JSON request,
unlock Bitwarden via the configured Bitwarden CLI executable, and expose a
small local API backed by the in-memory `BW_SESSION`.

## Prerequisites

- `cabal` and a compatible GHC toolchain
- `nc` for the manual socket examples below
- `XDG_RUNTIME_DIR` set in the environment

## Runtime contract

`hwarden-agent` requires an explicit Bitwarden CLI path via
`HWARDEN_BW_PATH`.

For the pinned Nix workflow in this repository, use `shell.nix`:

```sh
nix-shell
```

That shell pins `nixpkgs`, puts the wrapped `hwarden-agent` first on `PATH`,
and the wrapper sets `HWARDEN_BW_PATH` to the pinned Bitwarden CLI at runtime.
Inside the shell, running `hwarden-agent` uses that wrapped executable.

Outside Nix, set `HWARDEN_BW_PATH` yourself before starting the daemon. Having
some `bw` on `PATH` is not sufficient.

## NixOS module

This repository provides a small NixOS module at `nix/module.nix`. It installs
`hwarden-agent` as a systemd user service so the daemon keeps using the
per-user socket path:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
```

Example host configuration:

```nix
{
  imports = [
    /path/to/hwarden-agent/nix/module.nix
  ];

  services.hwarden-agent = {
    enable = true;
    serverUrl = "https://vault.bitwarden.eu";
    cacheRefreshIntervalSeconds = 60;
  };
}
```

Flake-based host configuration:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    hwarden-agent = {
      url = "github:skyvier/hbw-tools/add-nixos-module";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, hwarden-agent, ... }: {
    nixosConfigurations.example-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        hwarden-agent.nixosModules.default
        {
          services.hwarden-agent.enable = true;
        }
      ];
    };
  };
}
```

After switching the host configuration, start the service for a logged-in user:

```sh
systemctl --user enable --now hwarden-agent.service
```

The module builds a wrapped agent package by default. That package fixes the
Bitwarden CLI store path used by `hwarden-agent`; override
`services.hwarden-agent.package` with a differently built package if you need a
different CLI version. The main options are:

- `services.hwarden-agent.package`
- `services.hwarden-agent.serverUrl`
- `services.hwarden-agent.cacheRefreshIntervalSeconds`
- `services.hwarden-agent.extraEnvironment`

## CI/CD

GitHub Actions runs the CI/CD workflow in `.github/workflows/ci-cd.yml`.

For pull requests, the workflow runs:

- `cabal build all`
- `cabal test all`
- `test/scripts/run-all.sh`
- `hlint app src test`
- `fourmolu --config fourmolu.yaml --mode check app src test`

For pushes to `main`, the workflow runs the same checks and then publishes a
compressed Nix store closure artifact for the wrapped `hwarden-agent` package.
The artifact can be imported into another Nix store with:

```sh
zstd -dc hwarden-agent-<revision>-nix-closure.nar.zst | nix-store --import
```

Run the same checks locally from the pinned Nix shell:

```sh
nix-shell --pure shell.nix --run scripts/ci/check
```

Reformat Haskell source files from the pinned Nix shell:

```sh
nix-shell --pure shell.nix --run dev/format
```

## Run

Environment:

- `HWARDEN_BW_PATH`
  Required outside the pinned Nix shell. Must point to the `bw` executable to
  run for Bitwarden CLI commands.
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

If the derived socket path is longer than the UNIX socket pathname limit, the
daemon exits before creating runtime directories or starting the socket.

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
| `list-items` | `cmd` | `{"ok":true,"items":[...],"cache_age_seconds":0,"cache_refresh_status":"succeeded"}` | `{"ok":false,"error":"locked"}` or `{"ok":false,"error":"item cache unavailable"}` |
| `get-password` | `cmd`, `id` | `{"ok":true,"id":"...","password":"..."}` | `{"ok":false,"error":"locked"}` or `{"ok":false,"error":"..."}` |

Bad JSON returns `{"ok":false,"error":"..."}`. Unknown commands return
`{"ok":false,"error":"unknown request"}`.

## Minimal rofi frontend

This repository also includes a small shell-based frontend under `scripts/`.
It talks to the existing local agent socket and does not modify the backend.

Frontend dependencies:

- `rofi`
- `jq`
- `nc` with Unix socket support
- `xclip`

Main entrypoint:

```sh
scripts/hwarden-rofi
```

For demos, you can restrict the visible items by name prefix:

```sh
scripts/hwarden-rofi --prefix "Demo"
```

The prefix match is case-insensitive and only affects the frontend picker; it
does not change the backend `list-items` response.

The rofi frontend:

- checks whether the agent is already unlocked
- prompts for Bitwarden email and password when needed
- shows the item cache age and latest refresh result in the picker prompt
- lets you choose a login item from rofi
- copies the selected password to the X11 clipboard

Available helper commands:

- `scripts/hwarden-first-login`
- `scripts/hwarden-status`
- `scripts/hwarden-unlock`
- `scripts/hwarden-list-items`
- `scripts/hwarden-get-password`
- `scripts/hwarden-ensure-unlocked`
- `scripts/hwarden-copy-password`

If the Bitwarden CLI needs an interactive first-time login flow, use:

```sh
scripts/hwarden-first-login
```

That script:

- uses the agent's isolated `BITWARDENCLI_APPDATA_DIR`
- configures the same server as the agent
- runs interactive `bw login`
- logs out afterward so the agent can later start from its expected state

`scripts/hwarden-unlock` expects the email as its positional argument and the
secret values via environment variables:

```sh
HWARDEN_PASSWORD='MY_PASSWORD' scripts/hwarden-unlock me@example.com
```

`scripts/hwarden-get-password ITEM_ID` prints the plaintext password to stdout.
That is intentional for scripting, but `scripts/hwarden-rofi` and
`scripts/hwarden-copy-password` copy the password directly to the clipboard
instead of printing it.

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

If Bitwarden requires interactive two-factor setup for this CLI client, the
agent returns:

```json
{"ok":false,"error":"two-factor code required; run scripts/hwarden-first-login"}
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
  "cache_age_seconds": 0,
  "cache_refresh_status": "succeeded"
}
```

The agent warms this cache during a successful `unlock` request and refreshes
it in memory once a minute for the active unlocked session. If a background
refresh fails after at least one successful refresh, the agent keeps serving
the last cached item list, `cache_age_seconds` increases to show staleness, and
`cache_refresh_status` reports `failed`.

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
