# AGENTS

`hwarden-agent` is a tiny Haskell daemon with one job: accept a local Unix socket JSON request, run `bw login ... --raw`, and keep the resulting `BW_SESSION` only in process memory.

## Scope

- Preserve the single-purpose design.
- Prefer small, direct changes in `app/Main.hs`.
- Do not add persistence for session data, passwords, or other secrets.
- Keep the interface local-only over the Unix socket at `$XDG_RUNTIME_DIR/hwarden/agent.sock`.

## Workflow

- Build with `cabal build`.
- Run with `cabal run hwarden-agent`.
- `XDG_RUNTIME_DIR` must be set before running.
- For sandboxed environments, `HOME=/tmp cabal build` avoids Cabal config writes to a read-only home directory.

## Manual check

- Start the daemon.
- Send JSON over the socket with `socat`, for example an `unlock` request.
- Verify success returns `{"ok":true,"message":"unlocked"}`.
- Verify bad JSON or unknown commands return `{"ok":false,...}`.

## Change guidance

- Keep error messages free of leaked passwords.
- Maintain strict filesystem permissions for the socket directory.
- If behavior changes, update `README.md` with the new request or response contract.
