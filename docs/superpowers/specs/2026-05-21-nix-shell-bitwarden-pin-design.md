# Nix Shell and Bitwarden Pin Design

## Summary

The repository should stop depending on ambient `<nixpkgs>` and a manually
maintained Haskell dependency list in `shell.nix`. Instead, `shell.nix` should
pin a specific `nixpkgs` snapshot with `fetchTarball`, derive the local Haskell
package from `hwarden-agent.cabal` with `callCabal2nix`, and expose a dev shell
from that package.

The Bitwarden CLI used by the agent should also be pinned in Nix and passed to
the runtime as an explicit executable path. The supported Nix launch path
should wrap `hwarden-agent` so every `bw` invocation uses the exact pinned store
path instead of relying on `PATH`.

## Goals

- Pin `nixpkgs` inside the repository with a plain `fetchTarball`.
- Derive the Haskell package definition from `hwarden-agent.cabal` without any
  manual `cabal2nix` workflow.
- Keep `shell.nix` as the main development entrypoint.
- Pin a specific Bitwarden CLI package in Nix and ensure the agent receives its
  exact executable path at runtime.
- Keep the change small and aligned with the current single-purpose agent
  design.

## Non-goals

- This change does not convert the repository to flakes.
- This change does not replace Cabal as the day-to-day build tool for
  development and tests.
- This change does not broaden the agent API or expose arbitrary external
  command execution.
- This change does not add persistence for sessions, passwords, or other
  secrets.
- This change does not require the user to generate or check in a
  `cabal2nix`-produced Nix file.

## Architecture

### Pinned Nixpkgs source

`shell.nix` should own the `nixpkgs` pin directly:

- use `builtins.fetchTarball` with a fixed URL and hash
- import that pinned source instead of `<nixpkgs>`
- keep the pin local to the repository so developers get the same package set

This keeps the repository self-contained and avoids hidden dependency drift
from a caller's channel state.

### Cabal-derived Haskell package

The local Haskell package should be defined with:

- `pkgs.haskellPackages.callCabal2nix "hwarden-agent" ./. {}`

That package becomes the source of truth for Haskell dependencies in Nix. The
dev shell should be derived from it rather than manually listing library
dependencies. This removes the current duplication between `shell.nix` and
`hwarden-agent.cabal`.

The shell still needs a few non-Haskell tools alongside the package, such as
`cabal-install` and `socat`, but Haskell library selection should come from the
`.cabal` file.

### Bitwarden CLI pin and runtime injection

The same pinned package set should choose a specific Bitwarden CLI derivation.
The agent must receive that exact executable path at runtime, not just inherit a
shell `PATH` ordering.

The supported Nix path should therefore wrap the built `hwarden-agent`
executable and inject an environment variable containing the absolute store path
to the pinned `bw` binary. The wrapper should be part of the Nix package
definition so running the Nix-built executable is enough to get the correct
runtime configuration.

The development shell may still include the same Bitwarden CLI package in
`packages` for convenience, but the runtime guarantee comes from the wrapper,
not from shell search order.

### Runtime config boundary

The Haskell runtime already centralizes Bitwarden CLI configuration in
`Hwarden.App` and `Hwarden.Bitwarden.Real`. That boundary should be extended to
carry the executable path explicitly.

The environment/config should include:

- isolated Bitwarden CLI app-data directory
- Bitwarden server URL
- Bitwarden executable path

All Bitwarden process helpers should construct commands with the configured
executable path instead of `proc "bw" ...`.

## Error handling

### Missing executable path

Startup should fail early if the required Bitwarden executable path is absent
from the runtime environment. This should be treated as a configuration error,
not as a recoverable request-time failure.

The startup error should:

- clearly name the missing configuration
- avoid printing unrelated environment contents
- avoid leaking secrets

### Invalid executable path

If the configured path cannot be executed, existing request-time and startup
process error handling can continue to classify the failure as Bitwarden CLI
unavailability. No new secret-bearing error data should be introduced.

## README and developer workflow

`README.md` should document two distinct workflows:

- Cabal-driven development inside the pinned `nix-shell`
- the Nix-built wrapped executable path that injects the pinned `bw` location

The documentation should stop implying that having any `bw` on `PATH` is the
main runtime contract for the Nix-supported path. For non-Nix runs, the
required executable-path environment variable should be documented explicitly if
that path remains supported.

## Testing

The primary confidence should come from targeted unit and integration coverage:

1. configuration initialization reads the required Bitwarden executable path
2. startup fails cleanly when that path is missing
3. Bitwarden process creation uses the configured executable path instead of
   the literal `"bw"`
4. existing fake-CLI integration coverage still works when pointed at an
   explicit executable path

Nix expression evaluation is also part of the change. A practical verification
path is:

1. enter the pinned shell successfully
2. build with `cabal build`
3. run tests
4. build the wrapped Nix executable
5. confirm the wrapped executable starts with the expected runtime config shape

## Files likely to change

- `shell.nix`
  - pin `nixpkgs`
  - define the Cabal-derived Haskell package
  - expose a dev shell from that package
  - define a wrapped executable that injects the pinned `bw` path
- `src/Hwarden/App.hs`
  - load the Bitwarden executable path into `Env`
- `src/Hwarden/Bitwarden/Real.hs`
  - extend the config class with the executable path
  - use that path for all `proc` calls
- `README.md`
  - document the pinned Nix workflow and explicit runtime CLI path contract

## Risks and trade-offs

- Using `callCabal2nix` keeps Nix aligned with Cabal, but a package version
  available in Cabal may still be missing or constrained differently in the
  pinned `nixpkgs` snapshot. The pin should therefore be chosen for compatibility
  with the current `.cabal` file.
- A wrapped runtime path is stricter than relying on `PATH`, which is the
  intended outcome, but it means ad hoc non-Nix launches must either provide the
  same configuration variable or accept being unsupported.
- Keeping the dev shell and wrapped executable in one `shell.nix` is smaller
  than introducing a broader Nix packaging layout, but the file will take on
  both development-shell and packaging responsibilities.
