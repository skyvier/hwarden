#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

BW_LOG="$TMP_DIR/bw.log"
RUNTIME_DIR="$TMP_DIR/runtime"
CONFIG_DIR="$TMP_DIR/config"
SERVICE_APPDATA_DIR="$TMP_DIR/service-state/bitwarden-cli"

mkdir -p "$RUNTIME_DIR"
mkdir -p "$CONFIG_DIR"

cat >"$TMP_DIR/bw" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s|%s\n' "${BITWARDENCLI_APPDATA_DIR:-unset}" "$*" >>"$BW_LOG"
exit 0
EOF
chmod +x "$TMP_DIR/bw"

cat >"$TMP_DIR/systemctl" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF
chmod +x "$TMP_DIR/systemctl"

export BW_LOG
export HWARDEN_BW_PATH="$TMP_DIR/bw"
export XDG_RUNTIME_DIR="$RUNTIME_DIR"
export XDG_CONFIG_HOME="$CONFIG_DIR"
export PATH="$TMP_DIR:$PATH"

"$ROOT_DIR/scripts/hwarden-first-login"

expected_appdata="$CONFIG_DIR/hwarden/bitwarden-cli"

grep -F "$expected_appdata|logout" "$BW_LOG" >/dev/null
grep -F "$expected_appdata|config server https://vault.bitwarden.eu" "$BW_LOG" >/dev/null
grep -F "$expected_appdata|login" "$BW_LOG" >/dev/null

last_line=$(tail -n 1 "$BW_LOG")
[ "$last_line" = "$expected_appdata|logout" ]

cat >"$TMP_DIR/systemctl" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[ "$1" = "--user" ]
[ "$2" = "show" ]
[ "$3" = "hwarden-agent.service" ]
[ "$4" = "--property=Environment" ]
[ "$5" = "--value" ]
printf 'HWARDEN_SERVER_URL=https://vault.bitwarden.eu BITWARDENCLI_APPDATA_DIR=%s\n' "$SERVICE_APPDATA_DIR"
EOF
chmod +x "$TMP_DIR/systemctl"

export SERVICE_APPDATA_DIR

: >"$BW_LOG"

"$ROOT_DIR/scripts/hwarden-first-login"

grep -F "$SERVICE_APPDATA_DIR|logout" "$BW_LOG" >/dev/null
grep -F "$SERVICE_APPDATA_DIR|config server https://vault.bitwarden.eu" "$BW_LOG" >/dev/null
grep -F "$SERVICE_APPDATA_DIR|login" "$BW_LOG" >/dev/null

last_line=$(tail -n 1 "$BW_LOG")
[ "$last_line" = "$SERVICE_APPDATA_DIR|logout" ]
