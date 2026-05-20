# Rofi Prefix Filter Design

## Summary

Add an optional `--prefix <text>` flag to `scripts/hwarden-rofi` so the frontend only displays login items whose names start with the given prefix, matched case-insensitively. This is intended for safe demos where the operator wants to avoid showing personal login metadata.

## Goals

- Keep the feature entirely frontend-side.
- Preserve the existing `scripts/hwarden-rofi` behavior when no prefix is provided.
- Filter item visibility before the user sees the picker.
- Match prefixes case-insensitively.
- Avoid any backend or protocol changes.

## Non-Goals

- No changes to `hwarden-list-items` or the backend `list-items` response.
- No substring, regex, or fuzzy matching.
- No persistent demo profiles or configuration files.
- No filtering by username or other item fields.

## CLI Behavior

`scripts/hwarden-rofi` will support:

- `scripts/hwarden-rofi`
- `scripts/hwarden-rofi --prefix "Demo"`

Behavior rules:

- `--prefix` applies only to item names.
- Matching uses a case-insensitive prefix comparison.
- Non-matching items are excluded from the rofi picker.
- If no items match, the script shows a rofi error and exits nonzero.
- Unknown arguments should fail fast with a clear shell error.

## Implementation Shape

### `scripts/hwarden-rofi`

- Parse an optional `--prefix <text>` argument.
- Reject malformed or unknown arguments.
- Fetch the normal `list-items` payload.
- If a prefix is present, pass the item array through a helper before building labels.
- Continue using the filtered item array for:
  - label generation
  - selected index lookup
  - final item-id resolution

### `scripts/hwarden-lib`

Add a small helper responsible only for name-prefix filtering, for example:

- accept a JSON item array and a prefix
- return a filtered JSON item array
- implement matching with `jq` using `ascii_downcase` plus `startswith`

This keeps JSON filtering logic out of the main rofi script while avoiding a broader abstraction.

## Error Handling

- If the prefix flag is missing its value, fail immediately.
- If filtering yields zero items, show a rofi error such as `no login items match prefix: Demo`.
- Existing agent/clipboard/dependency failures remain unchanged.

## Testing

Add one new frontend shell regression test that:

- stubs `list-items` with mixed item names
- invokes `scripts/hwarden-rofi --prefix ...`
- verifies only matching items are shown to rofi
- verifies case-insensitive behavior

Retain existing frontend shell tests unchanged.

Verification should include:

- `bash test/frontend-rofi-progress.sh`
- new prefix-filter shell test
- `bash -n` for touched scripts/tests
- `HOME=/tmp cabal test`

## Rationale

The feature stays local to the rofi frontend because the goal is presentation safety during demos, not a new agent capability. Frontend-only filtering minimizes review scope and avoids coupling demo behavior to the backend API.
