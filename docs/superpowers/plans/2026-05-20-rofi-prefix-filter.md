# Rofi Prefix Filter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an optional case-insensitive `--prefix` filter to `scripts/hwarden-rofi` so demo users can hide unrelated login-item names from the picker.

**Architecture:** Keep the feature entirely frontend-side. `scripts/hwarden-rofi` will parse the optional CLI flag and use a small helper in `scripts/hwarden-lib` to filter the `list-items` JSON array before building rofi labels. Add one shell regression test that proves the filter is case-insensitive and only exposes matching names.

**Tech Stack:** Bash, jq, rofi shell test harness, existing frontend helper scripts

---

## File Structure

- Modify: `scripts/hwarden-rofi`
  - parse the optional `--prefix <text>` argument
  - apply filtered item JSON before label generation and selection lookup
- Modify: `scripts/hwarden-lib`
  - add a focused helper for case-insensitive name-prefix filtering on an item JSON array
- Create: `test/frontend-prefix-filter.sh`
  - regression test for prefix filtering behavior and rofi-visible labels
- Modify: `README.md`
  - document the optional `--prefix` flag for demo usage

### Task 1: Add the filtering helper

**Files:**
- Modify: `scripts/hwarden-lib`
- Test: `test/frontend-prefix-filter.sh`

- [ ] **Step 1: Write the failing shell regression test scaffold**

Create `test/frontend-prefix-filter.sh` with this content:

```bash
#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

export HWARDEN_SKIP_SOCKET_CHECK=1
export HWARDEN_SOCKET_PATH="$TMP_DIR/agent.sock"

cat > "$TMP_DIR/response.txt" <<'JSON'
{"ok":true,"items":[
  {"id":"1","name":"Demo Mail","username":"demo@example.com"},
  {"id":"2","name":"Personal Bank","username":"me@example.com"},
  {"id":"3","name":"demo Admin","username":"root@example.com"}
]}
JSON

cat > "$TMP_DIR/nc" <<EOF2
#!/usr/bin/env bash
cat "$TMP_DIR/response.txt"
EOF2
chmod +x "$TMP_DIR/nc"

cat > "$TMP_DIR/rofi" <<'EOF2'
#!/usr/bin/env bash
cat > "$TMP_DIR/labels.txt"
printf '0\n'
EOF2
chmod +x "$TMP_DIR/rofi"

cat > "$TMP_DIR/xclip" <<'EOF2'
#!/usr/bin/env bash
cat > /dev/null
EOF2
chmod +x "$TMP_DIR/xclip"

export PATH="$TMP_DIR:$PATH"

"$REPO_DIR/scripts/hwarden-rofi" --prefix demo >/dev/null 2>"$TMP_DIR/stderr.txt"

expected=$'Demo Mail [demo@example.com]\ndemo Admin [root@example.com]'
actual=$(cat "$TMP_DIR/labels.txt")

[ "$actual" = "$expected" ]
```

- [ ] **Step 2: Run the new test to verify it fails**

Run:

```bash
bash test/frontend-prefix-filter.sh
```

Expected: failure because `scripts/hwarden-rofi` does not recognize `--prefix` yet.

- [ ] **Step 3: Add the filtering helper to `scripts/hwarden-lib`**

Append this helper near the existing JSON request/response helpers:

```bash
hwarden_filter_items_by_prefix() {
  local items_json=$1
  local prefix=$2

  printf '%s' "$items_json" |
    jq -c --arg prefix "$prefix" '
      [ .[]
        | select((.name | ascii_downcase) | startswith($prefix | ascii_downcase))
      ]
    '
}
```

- [ ] **Step 4: Run the shell syntax check for the helper file**

Run:

```bash
bash -n scripts/hwarden-lib
```

Expected: no output, exit code 0.

- [ ] **Step 5: Commit the helper foundation**

```bash
git add scripts/hwarden-lib test/frontend-prefix-filter.sh
git commit -m "Add rofi item prefix filter helper"
```

### Task 2: Wire `--prefix` into `scripts/hwarden-rofi`

**Files:**
- Modify: `scripts/hwarden-rofi`
- Test: `test/frontend-prefix-filter.sh`

- [ ] **Step 1: Extend the failing test to cover the empty-match error**

Append this block to `test/frontend-prefix-filter.sh` after the existing successful assertion:

```bash
if "$REPO_DIR/scripts/hwarden-rofi" --prefix missing >/dev/null 2>"$TMP_DIR/missing.err"; then
  echo "expected prefix-miss invocation to fail" >&2
  exit 1
fi

grep -q 'no login items match prefix: missing' "$TMP_DIR/missing.err"
```

- [ ] **Step 2: Update `scripts/hwarden-rofi` to parse `--prefix` and filter items**

Change the top of `scripts/hwarden-rofi` to this structure:

```bash
#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=scripts/hwarden-lib
source "$SCRIPT_DIR/hwarden-lib"

prefix_filter=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --prefix)
      [ "$#" -ge 2 ] || hwarden_fail "missing value for --prefix"
      prefix_filter=$2
      shift 2
      ;;
    *)
      hwarden_fail "unknown argument: $1"
      ;;
  esac
done

hwarden_require_rofi_dependencies
hwarden_require_x11_clipboard

"$SCRIPT_DIR/hwarden-ensure-unlocked"

items_json=$("$SCRIPT_DIR/hwarden-list-items")
if [ -n "$prefix_filter" ]; then
  items_json=$(hwarden_filter_items_by_prefix "$items_json" "$prefix_filter")
fi

item_count=$(printf '%s' "$items_json" | jq 'length')
[ "$item_count" -gt 0 ] || {
  if [ -n "$prefix_filter" ]; then
    hwarden_rofi_error "no login items match prefix: $prefix_filter"
  else
    hwarden_rofi_error "no login items available"
  fi
  exit 1
}
```

Keep the remainder of the script unchanged so it uses the filtered `items_json` for labels and item-id lookup.

- [ ] **Step 3: Run the targeted frontend tests**

Run:

```bash
bash test/frontend-prefix-filter.sh
bash test/frontend-rofi-progress.sh
```

Expected: both pass.

- [ ] **Step 4: Run shell syntax checks for touched scripts/tests**

Run:

```bash
bash -n scripts/hwarden-lib scripts/hwarden-rofi test/frontend-prefix-filter.sh
```

Expected: no output, exit code 0.

- [ ] **Step 5: Commit the CLI wiring**

```bash
git add scripts/hwarden-rofi scripts/hwarden-lib test/frontend-prefix-filter.sh
git commit -m "Support filtered rofi item picker"
```

### Task 3: Document and verify the full change

**Files:**
- Modify: `README.md`
- Test: `test/frontend-prefix-filter.sh`

- [ ] **Step 1: Update the README frontend section**

Add a short usage note near the `scripts/hwarden-rofi` docs:

```markdown
For demos, you can restrict the visible items by name prefix:

```bash
scripts/hwarden-rofi --prefix "Demo"
```

The prefix match is case-insensitive and only affects the frontend picker; it does not change the backend `list-items` response.
```

- [ ] **Step 2: Run the full frontend shell regression set**

Run:

```bash
bash test/frontend-first-login.sh
bash test/frontend-rofi-progress.sh
bash test/frontend-otp-workflow.sh
bash test/frontend-prefix-filter.sh
```

Expected: all pass.

- [ ] **Step 3: Run the full project test suite**

Run:

```bash
/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test'
```

Expected: all tests pass, including Unix socket integration tests.

- [ ] **Step 4: Inspect the diff summary**

Run:

```bash
git diff --stat HEAD~3..HEAD
```

Expected: only `README.md`, `scripts/hwarden-lib`, `scripts/hwarden-rofi`, and `test/frontend-prefix-filter.sh` should reflect this feature work.

- [ ] **Step 5: Commit the docs/test completion**

```bash
git add README.md test/frontend-prefix-filter.sh
git commit -m "Document rofi demo prefix filter"
```
