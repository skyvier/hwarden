#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

BW_LOG="$TMP_DIR/bw.log"
RUNTIME_DIR="$TMP_DIR/runtime"

mkdir -p "$RUNTIME_DIR"

cat >"$TMP_DIR/bw" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s|%s\n' "${BITWARDENCLI_APPDATA_DIR:-unset}" "$*" >>"$BW_LOG"
exit 0
EOF
chmod +x "$TMP_DIR/bw"

export PATH="$TMP_DIR:$PATH"
export BW_LOG
export XDG_RUNTIME_DIR="$RUNTIME_DIR"

"$ROOT_DIR/scripts/hwarden-first-login"

expected_appdata="$RUNTIME_DIR/hwarden/bitwarden-cli"

grep -F "$expected_appdata|logout" "$BW_LOG" >/dev/null
grep -F "$expected_appdata|config server https://vault.bitwarden.eu" "$BW_LOG" >/dev/null
grep -F "$expected_appdata|login" "$BW_LOG" >/dev/null

last_line=$(tail -n 1 "$BW_LOG")
[ "$last_line" = "$expected_appdata|logout" ]
