# hwarden-agent

<div align="center">

https://github.com/user-attachments/assets/2a3344e5-dd19-4e8c-ac6f-3d1f2f730a51

</div>

`hwarden-agent` is a small Haskell daemon for using Bitwarden CLI from local automation without handing every caller direct access to `BW_SESSION` or the user's normal Bitwarden CLI profile.

The daemon listens on a local Unix socket, accepts a narrow JSON protocol, unlocks Bitwarden CLI with `bw login --raw`, and keeps the resulting session key only in process memory. Responses never include `BW_SESSION`.

It is intended for trusted local tools such as shell scripts, launchers, and rofi frontends. It is not a network service and is not designed to protect against malicious code already running as the same user.

By default, the agent uses:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
```

and an isolated Bitwarden CLI profile under:

```text
$XDG_CONFIG_HOME/hwarden/bitwarden-cli
```

## Motivation

Bitwarden CLI is powerful, but many automation workflows end up exporting, copying, or reusing `BW_SESSION` directly. That makes the session easier to leak through shell history, logs, process environments, or helper scripts.

`hwarden-agent` keeps the session in one long-running local process instead. Callers unlock the agent once, then use a small local API for common operations such as checking status, listing login items, and retrieving a selected password.

The goal is not to make local untrusted code safe. The goal is to reduce accidental session exposure, keep the Bitwarden CLI state isolated, and provide a small auditable interface for local automation.

The common user path can then be a thin frontend such as:

```text
scripts/hwarden-rofi
```

## Security model

`hwarden-agent` reduces accidental exposure of Bitwarden CLI state. It does not make a compromised user session safe.

The daemon:

* keeps `BW_SESSION` in process memory
* never returns `BW_SESSION` over the socket
* uses an isolated Bitwarden CLI profile
* exposes only a small local command set
* sanitizes logs and responses to avoid leaking sensitive values
* creates its runtime and CLI profile directories with restrictive permissions

However, any process that can connect to the agent socket can use the supported API while the agent is unlocked. In the default setup, the socket lives under `$XDG_RUNTIME_DIR/hwarden` and is intended to be reachable only by the owning user.

The intended threat model is:

* avoid exporting or copying `BW_SESSION` into shell scripts
* avoid sharing the user's normal Bitwarden CLI profile with automation
* avoid session leakage through process environments, logs, and helper scripts
* keep the local API narrow enough to reason about

The agent is not intended to protect against malicious code already running as the same user.

## Quick start

Build and run the pinned package:

```sh
nix build .#hwarden-agent
./result/bin/hwarden-agent
```

In another terminal, check the agent status:

```sh
printf '{"cmd":"status"}' | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

For the rofi frontend:

```sh
scripts/hwarden-rofi
```

On NixOS, enable the user service with the provided module:

```nix
{
  services.hwarden-agent.enable = true;
}
```

Then start the service for the logged-in user:

```sh
systemctl --user enable --now hwarden-agent.service
```

## Requirements

For the daemon:

* Linux or another Unix-like system with Unix-domain sockets
* `XDG_RUNTIME_DIR`
* Bitwarden CLI, supplied either by the flake wrapper or by `HWARDEN_BW_PATH`

For development:

* Nix
* Cabal inside the pinned development shell

For the rofi frontend:

* `rofi`
* `jq`
* `nc` with Unix socket support
* `wl-copy` from `wl-clipboard` on Wayland
* `xclip` on X11

## Pinned builds

This repository pins its dependencies in the flake lock file. Use the build products or shell from this repository instead of pointing Cabal at a system `nixpkgs`.

Build the wrapped package from the flake:

```sh
nix build .#hwarden-agent
./result/bin/hwarden-agent
```

For development, use the pinned shell defined in this repository:

```sh
nix-shell --pure shell.nix
cabal build
cabal run hwarden-agent
```

The wrapped package and the shell both set `HWARDEN_BW_PATH` to the pinned Bitwarden CLI from this repository.

Outside those environments, set `HWARDEN_BW_PATH` yourself to the `bw` executable you want the daemon to run.

## Runtime behavior

`hwarden-agent` requires `XDG_RUNTIME_DIR` to be set.

The daemon creates these directories if needed and forces them to mode `0700`:

```text
$XDG_RUNTIME_DIR/hwarden
$XDG_CONFIG_HOME/hwarden/bitwarden-cli
```

The isolated CLI profile contains Bitwarden CLI app data. The unlocked `BW_SESSION` is kept only in daemon memory.

If `BITWARDENCLI_APPDATA_DIR` is set, it must be an absolute path and it overrides the default Bitwarden CLI profile directory.

If `BITWARDENCLI_APPDATA_DIR` is not set, the daemon uses:

```text
$XDG_CONFIG_HOME/hwarden/bitwarden-cli
```

falling back to:

```text
$HOME/.config/hwarden/bitwarden-cli
```

At startup the daemon:

1. removes any stale socket at `$XDG_RUNTIME_DIR/hwarden/agent.sock`
2. configures the Bitwarden CLI server in its isolated profile
3. logs out any leftover CLI state before accepting requests

If the derived socket path exceeds the Unix pathname limit, startup fails before the socket is created.

After a successful unlock, the daemon starts an in-memory item cache refresh loop. The refresh interval is controlled by `HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS`.

If a later background refresh fails after the cache is already available, `list-items` can still return the previous cached items and report the failed refresh status.

## Environment variables

### `HWARDEN_BW_PATH`

Required outside the pinned shell or wrapped package.

Must point to the `bw` executable the daemon should run.

### `HWARDEN_SERVER_URL`

Defaults to:

```text
https://vault.bitwarden.eu
```

### `HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS`

Defaults to `60`.

Invalid or non-positive values fall back to `60`.

This controls the in-memory refresh loop that starts after a successful unlock.

### `BITWARDENCLI_APPDATA_DIR`

Optional.

If set, it must be an absolute path and is used as the isolated Bitwarden CLI profile directory.

If unset, the daemon uses the default hwarden profile directory.

## NixOS module

The NixOS module lives in:

```text
nix/module.nix
```

and is exported from the flake as:

```text
hwarden-agent.nixosModules.default
```

The module installs `hwarden-agent` as a systemd user service and keeps the socket at the standard per-user path:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
```

The module's default package wraps the daemon with the pinned Bitwarden CLI from this repository. That is the package the service runs by default.

Do not expect `HWARDEN_BW_PATH` to be supplied by the service environment itself when using the default wrapped package.

Example:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hwarden-agent = {
      url = "github:skyvier/hbw-tools";
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

Leave `hwarden-agent` to use its own locked `nixpkgs` so the package and module build against the pinned dependencies from this repository.

Do not override the `hwarden-agent` input's `nixpkgs` unless you intentionally want to rebuild it against a different dependency set.

### Module options

```text
services.hwarden-agent.enable
services.hwarden-agent.package
services.hwarden-agent.serverUrl
services.hwarden-agent.cacheRefreshIntervalSeconds
services.hwarden-agent.extraEnvironment
```

### Service environment

The user service sets these environment variables:

```text
HWARDEN_SERVER_URL
HWARDEN_CACHE_REFRESH_INTERVAL_SECONDS
BITWARDENCLI_APPDATA_DIR=%S/hwarden-agent/bitwarden-cli
```

### Service hardening

The service uses these settings:

```text
RuntimeDirectory=hwarden
StateDirectory=hwarden-agent
ProtectHome=read-only
ProtectSystem=strict
UMask=0077
```

### Starting and stopping the service

After switching to the module, start the service for the logged-in user:

```sh
systemctl --user enable --now hwarden-agent.service
```

Restart it with:

```sh
systemctl --user restart hwarden-agent.service
```

Stop it with:

```sh
systemctl --user stop hwarden-agent.service
```

View logs with:

```sh
journalctl --user -u hwarden-agent.service -f
```

## Rofi frontend

Most users will interact with the agent through:

```text
scripts/hwarden-rofi
```

It is a small shell frontend that talks to the local socket, unlocks the agent when needed, shows the login-item picker in rofi, and copies the chosen password to the clipboard.

Start it directly:

```sh
scripts/hwarden-rofi
```

Optional prefix filtering is available for larger vaults:

```sh
scripts/hwarden-rofi --prefix Demo
```

The frontend prompts for Bitwarden email and password only when the agent is locked. If the agent is already unlocked, it goes straight to the picker.

The frontend uses the same local socket as the daemon and does not change backend behavior.

### Frontend dependencies

```text
rofi
jq
nc with Unix socket support
wl-copy from wl-clipboard on Wayland
xclip on X11
```

### Supporting scripts

```text
scripts/hwarden-first-login
scripts/hwarden-status
scripts/hwarden-unlock
scripts/hwarden-list-items
scripts/hwarden-get-password
scripts/hwarden-ensure-unlocked
scripts/hwarden-copy-password
```

## First login and two-factor authentication

The normal `unlock` request uses `bw login --raw`.

If Bitwarden requires a two-factor code, the agent returns:

```json
{"ok":false,"error":"two-factor code required; run scripts/hwarden-first-login"}
```

In that case, run:

```sh
scripts/hwarden-first-login
```

After the first-login flow has prepared the isolated Bitwarden CLI profile, the normal agent unlock path can be used again.

## Protocol

Clients connect to:

```text
$XDG_RUNTIME_DIR/hwarden/agent.sock
```

A client sends one JSON object, closes the write side of the connection, and then reads one JSON response.

The daemon reads until EOF before decoding the request.

Supported commands:

```text
unlock
status
list-items
get-password
```

### Request examples

```json
{"cmd":"unlock","email":"me@example.com","password":"MY_PASSWORD"}
{"cmd":"status"}
{"cmd":"list-items"}
{"cmd":"get-password","id":"1"}
```

### `unlock`

Successful unlock:

```json
{"ok":true,"message":"unlocked"}
```

Already unlocked:

```json
{"ok":true,"message":"already unlocked"}
```

Common failures:

```json
{"ok":false,"error":"bw login failed"}
```

```json
{"ok":false,"error":"two-factor code required; run scripts/hwarden-first-login"}
```

### `status`

Locked:

```json
{"ok":true,"message":"locked"}
```

Unlocked:

```json
{"ok":true,"message":"unlocked"}
```

### `list-items`

Success:

```json
{
  "ok": true,
  "items": [],
  "cache_age_seconds": 0,
  "cache_refresh_status": "succeeded"
}
```

Locked:

```json
{"ok":false,"error":"locked"}
```

Cache not ready yet:

```json
{"ok":false,"error":"item cache unavailable"}
```

If a background refresh fails after the cache is ready, cached items are still returned with `cache_refresh_status` set to `failed`.

### `get-password`

Success:

```json
{"ok":true,"id":"...","password":"..."}
```

Locked:

```json
{"ok":false,"error":"locked"}
```

### Bad requests

Bad JSON returns a decode error in the `error` field.

Unknown commands return:

```json
{"ok":false,"error":"unknown request"}
```

## Manual checks

Start the daemon, then send a request with `nc`:

```sh
printf '{"cmd":"status"}' | nc -N -U "$XDG_RUNTIME_DIR/hwarden/agent.sock"
```

An unlock request should return:

```json
{"ok":true,"message":"unlocked"}
```

Bad JSON and unknown commands should return:

```json
{"ok":false,...}
```

## Testing

Run the test suite with:

```sh
HOME=/tmp cabal test
```

The test suite covers:

* request routing
* state machine transitions
* log sanitization
* response sanitization
* JSON codecs
* socket-level integration paths
* startup behavior
* unlock behavior
* refresh behavior
* shutdown behavior

## Limitations

`hwarden-agent` is intentionally small and local.

Current limitations:

* The protocol is request/response based.
* Each client request sends one JSON object and receives one JSON response.
* Two-factor login is not handled by the normal unlock request.
* Any process that can access the socket can use the unlocked agent.
* The item cache may briefly be unavailable immediately after unlock.
* The project currently targets local Unix-like environments, especially NixOS-style setups.

## Repository layout

```text id="mqdm1a"
src/Hwarden/                  Daemon implementation
app/Main.hs                   Executable entry point
scripts/                      Shell frontends and helper commands
scripts/ci/                   CI helper scripts
dev/                          Development helper scripts
nix/                          Nix package, pinned package set, and NixOS module
test/                         Haskell, shell, Nix, integration, and golden tests
```

Important files:

```text id="5xt1c7"
src/Hwarden/Agent.hs          Agent state machine and request handling
src/Hwarden/Bitwarden.hs      Bitwarden CLI interface
src/Hwarden/Bitwarden/Real.hs Real Bitwarden CLI implementation
src/Hwarden/Cache.hs          In-memory item cache and refresh logic
src/Hwarden/Runtime.hs        Runtime directory and environment setup
src/Hwarden/Socket.hs         Unix socket server
src/Hwarden/Logging.hs        Type-indexed structured logging API
src/Hwarden/Sanitize.hs       Secret redaction and sanitization
src/Hwarden/Response.hs       JSON response construction
src/Hwarden/Types.hs          Shared domain and protocol types

scripts/hwarden-rofi          Rofi password picker frontend
scripts/hwarden-lib           Shared shell helper functions
scripts/hwarden-first-login   First-login / two-factor helper

nix/package.nix               Nix package definition
nix/module.nix                NixOS module
```

`src/Hwarden/Logging.hs` contains the type-indexed logging layer used by the daemon. Log events are represented as structured values instead of ad-hoc strings, and the type-level event names make it harder to accidentally introduce unreviewed or inconsistent log messages.

This works together with `src/Hwarden/Sanitize.hs` and the sanitization tests to keep secrets out of logs, JSON responses, exceptions, and rendered values.

Relevant tests:

```text id="97duxu"
test/Test/Logging.hs                  Logging behavior
test/Test/ExceptionLogging.hs         Exception logging behavior
test/Test/Sanitization.hs             General sanitization behavior
test/Test/Sanitization/Json.hs        JSON sanitization
test/Test/Sanitization/Log.hs         Log sanitization
test/Test/Sanitization/Show.hs        Show-instance leakage checks
test/Test/Sanitization/ToLog.hs       ToLog-instance leakage checks
test/Test/Sanitization/LeakingMockEnv.hs  Negative/control cases for leakage tests
```
