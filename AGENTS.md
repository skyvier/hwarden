# AGENTS

`hwarden-agent` is a tiny Haskell daemon with one job: accept a local Unix socket JSON request, run `bw login ... --raw`, and keep the resulting `BW_SESSION` only in process memory.

## Scope

- Preserve the single-purpose design.
- Prefer small, direct changes 
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

- Keep error messages free of leaked passwords or session keys.
- Maintain strict filesystem permissions for the socket directory.
- If behavior changes, update `README.md` with the new request or response contract.

## Coding conventions

### Type classes

- No orpahed type class instances without explicit approval from the developer
- Type class instances must be placed directly under the type definition

#### Arbitrary instances

- If you need an Arbitrary instance for a type
    - Prefer defining the instance in the same module that defines the original type
    - If the instance needs to be specialized, use a newtype wrapper and define 
      the Arbitrary instance for the newtype wrapper
    - Always implement the shrink function
        - If it makes sense, rely on genericShrink


### Testing

- If you define roundtrip transformations (e.g. ToJSON/FromJSON, Show/Read), 
  write roundtrip property tests for the relevant types
- If you define a new ToJSON instance, write golden tests that test the encoding
- If you define a new FromJSON instance, write decode tests that cover the logic
  (including edge cases)

## Test strategy

- Prefer tests with one primary purpose.
- Use `decide` tests for request routing by agent state.
- Use handler/unit tests for state transitions, backend result mapping, and
  exact time-dependent behavior.
- Use integration tests only for end-to-end daemon lifecycles such as startup,
  unlock, cache warmup, background refresh, and socket responses.
- Do not re-test unrelated invariants under every new feature dimension.
- Before adding a test, state what new behavior it proves that existing tests
  do not already prove.

## Git commit messages

When writing Git commit messages, follow this format exactly:

- The first line is the title.
- Ideally, the title should be less than 50 characters.
    - Hard limit is 72 characters
- The title must be in imperative mood and fit this sentence:

  "This commit will <title>"

  Good examples:
  - Clean up unused imports
  - Introduce SIGTERM handler
  - Add Bitwarden item lookup logging

- After the title, write exactly one empty line.
- Then write a body with one concise paragraph per topic.
- Wrap all body lines at less than 72 characters.
- The body should answer these questions when relevant:
  - What was the previous behavior?
  - How does this commit change the behavior?
  - What implementation details are worth remembering?

Do not use a one-line commit message unless the change is truly trivial.
Do not include bullet points unless they make the body substantially clearer.
Do not include literal "\n" sequences. Use real newlines.
