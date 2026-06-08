#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

ROFI_LOG="$TMP_DIR/rofi.log"
WORK_LOG="$TMP_DIR/work.log"
FAIL_LOG="$TMP_DIR/fail.log"

cat >"$TMP_DIR/rofi" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >>"$ROFI_LOG"
exit 0
EOF
chmod +x "$TMP_DIR/rofi"

cat >"$TMP_DIR/work" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'copied\n' >>"$WORK_LOG"
EOF
chmod +x "$TMP_DIR/work"

cat >"$TMP_DIR/fail-work" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'failed\n' >>"$FAIL_LOG"
exit 1
EOF
chmod +x "$TMP_DIR/fail-work"

export PATH="$TMP_DIR:$PATH"
export ROFI_LOG WORK_LOG FAIL_LOG

# shellcheck source=scripts/hwarden-lib
source "$ROOT_DIR/scripts/hwarden-lib"

hwarden_rofi_progress \
  "Loading password..." \
  "Password copied" \
  "Password copy failed" \
  "$TMP_DIR/work"

grep -F "Loading password..." "$ROFI_LOG" >/dev/null
grep -F "Password copied" "$ROFI_LOG" >/dev/null
grep -F -- "-theme-str window { width: 800px; } -e Loading password..." "$ROFI_LOG" >/dev/null
grep -F -- "-theme-str window { width: 800px; } -e Password copied" "$ROFI_LOG" >/dev/null
grep -F "copied" "$WORK_LOG" >/dev/null

if hwarden_rofi_progress \
  "Loading password..." \
  "Password copied" \
  "Password copy failed" \
  "$TMP_DIR/fail-work"; then
  printf '%s\n' 'expected failing work command to fail' >&2
  exit 1
fi

grep -F "Password copy failed" "$ROFI_LOG" >/dev/null
grep -F -- "-theme-str window { width: 800px; } -e Password copy failed" "$ROFI_LOG" >/dev/null
grep -F "failed" "$FAIL_LOG" >/dev/null
