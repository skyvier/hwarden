# Email OTP Unlock Design

## Summary

The agent should support Bitwarden email-based two-factor login during `unlock`. The socket request will gain an optional `twoFactorCode` field, and the real Bitwarden backend will use it to invoke `bw login ... --method 1 --code <code>` when present.

When the code is not present, the login path must remain non-interactive and fail instead of hanging if Bitwarden expects an OTP prompt.

## Goals

- Let first-time or re-verified CLI clients unlock through the agent when Bitwarden requires an email OTP.
- Keep the request interface small by supporting only the email OTP variant for now.
- Prevent `bw login` from hanging indefinitely when extra authentication is required but no code is provided.
- Keep secrets redacted in logs and error handling.

## Non-goals

- This change does not support arbitrary Bitwarden 2FA methods.
- This change does not add generic `twoFactorMethod` selection to the API.
- This change does not alter the existing `status`, `list-items`, or `get-password` request contracts.
- This change does not persist OTPs or any other new secret material.

## Request and Response Contract

### Unlock request

The existing unlock request is extended with an optional `twoFactorCode` field:

```json
{
  "cmd": "unlock",
  "email": "john@example.com",
  "password": "secret",
  "twoFactorCode": "249213"
}
```

If `twoFactorCode` is omitted, the request remains valid and preserves current behavior except that the backend must now fail fast instead of hanging if Bitwarden expects interactive OTP entry.

### Unlock response

The success response stays unchanged:

```json
{"ok":true,"message":"unlocked"}
```

Failure responses also stay in the existing shape:

```json
{"ok":false,"error":"..."}
```

The error text should clearly indicate when a two-factor code is required but missing.

## Types and Boundaries

### New secret type

Add a `TwoFactorCode` type in `Hwarden.Types`, following the same approach as `Password` and `PasswordValue`:

- wrap `Text`
- redact via `Show`
- do not leak the code in logs or derived request rendering

### Request model

Extend `UnlockRequest` so it carries:

- `Username`
- `Password`
- `Maybe TwoFactorCode`

This keeps the interface explicit and avoids overloading arbitrary request metadata.

### Bitwarden effect boundary

Extend the `Bitwarden` class so `unlock` accepts an optional OTP:

```haskell
unlock :: Username -> Password -> Maybe TwoFactorCode -> m (Either UnlockError SessionKey)
```

This change belongs in the effect boundary because the unlock command semantics really have changed.

## Real CLI Behavior

### With `twoFactorCode`

When the request includes `twoFactorCode`, the real backend should run:

```text
bw login <email> <password> --raw --method 1 --code <code>
```

Method `1` is hardcoded because this first version supports only the email OTP path.

### Without `twoFactorCode`

When the request does not include a code, the backend should still run login non-interactively, but it must fail instead of waiting indefinitely for input.

The practical requirement is:

- do not allow an interactive OTP prompt to hang the agent
- return a normal unlock failure instead

This likely means the login subprocess path should stop using the generic `readCreateProcessWithExitCode` helper and instead use explicit `createProcess` wiring with:

- closed or otherwise non-interactive stdin
- captured stdout/stderr
- a timeout around process completion

The implementation detail can be chosen during coding, but the observable behavior must be “fast failure, no hang”.

## Error Handling

### Missing OTP

If Bitwarden requires email OTP and none was supplied:

- do not hang
- return a failure message such as `"two-factor code required"`

### Invalid OTP

If a code was supplied but Bitwarden rejects it:

- return the sanitized Bitwarden failure text in the usual unlock failure path

### Timeout

If the CLI still blocks despite the non-interactive setup:

- treat that as an unlock failure
- return a message that points to missing additional authentication rather than surfacing raw process-control jargon

## Logging

The new `twoFactorCode` must be treated as secret:

- `Show` must redact it
- request logs must not reveal it
- backend logs must not include it in command text or structured fields

The existing request logging based on `Show Request` should remain safe once the new type is redacted.

## Testing

### Pure tests

Prefer property tests where the behavior is invariant-based.

Coverage should include:

- parsing unlock requests with and without `twoFactorCode`
- request encoding with and without `twoFactorCode`
- `Show` on unlock requests does not expose the OTP

### Integration tests

The fake `bw` harness should cover:

1. unlock succeeds without OTP when the backend does not require one
2. unlock succeeds with `twoFactorCode`
3. unlock without `twoFactorCode` fails fast when the backend requires OTP
4. unlock with an invalid OTP fails normally

The “fails fast” test should be designed so it proves lack of indefinite waiting pragmatically without making the harness overly complex.

## Documentation

`README.md` must be updated to say:

- `twoFactorCode` is an optional unlock field
- only email OTP is supported for now
- the method is hardcoded internally to Bitwarden’s email 2FA path
- missing OTP no longer causes a hang; it returns an unlock failure instead

## Files likely to change

- `src/Hwarden/Types.hs`
  - add `TwoFactorCode`
- `src/Hwarden/Agent.hs`
  - extend `UnlockRequest` parsing/encoding
- `src/Hwarden/Bitwarden.hs`
  - update `unlock` signature
- `src/Hwarden/Bitwarden/Real.hs`
  - add email-OTP CLI argument support
  - make unlock non-interactive and failure-bounded
- `test/Main.hs`
  - add pure request/logging coverage
- `test/Integration.hs`
  - extend fake `bw` login behavior for OTP flows
- `README.md`
  - document the new unlock contract

## Risks and Trade-offs

- Hardcoding email OTP is intentionally narrow; it solves the immediate onboarding problem without expanding the API too early.
- Detecting “OTP required” from the CLI may depend on stderr text or timeout behavior, so the implementation should prefer a clearly non-interactive subprocess setup rather than brittle message parsing alone.
- The timeout path should be conservative and documented, because it becomes part of the unlock UX for misconfigured or newly challenged clients.
