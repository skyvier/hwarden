#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

REQUEST_LOG="$TMP_DIR/requests.log"
ROFI_LOG="$TMP_DIR/rofi.log"
ROFI_RESPONSES="$TMP_DIR/rofi-responses.txt"
export HWARDEN_SOCKET_PATH="$TMP_DIR/fake.sock"
export HWARDEN_SKIP_SOCKET_CHECK=1

cat >"$ROFI_RESPONSES" <<'EOF'
me@example.com
good-password
EOF

cat >"$TMP_DIR/rofi" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >>"$ROFI_LOG"
if [ "${1:-}" = "-e" ]; then
  exit 0
fi
head -n 1 "$ROFI_RESPONSES"
tail -n +2 "$ROFI_RESPONSES" >"$ROFI_RESPONSES.tmp"
mv "$ROFI_RESPONSES.tmp" "$ROFI_RESPONSES"
EOF
chmod +x "$TMP_DIR/rofi"

cat >"$TMP_DIR/nc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
payload=$(cat)
printf '%s\n' "$payload" >>"$REQUEST_LOG"
case "$payload" in
  *'"cmd":"status"'*)
    printf '%s' '{"ok":true,"message":"locked"}'
    ;;
  *'"cmd":"unlock"'*)
    printf '%s' '{"ok":false,"error":"two-factor code required; run scripts/hwarden-first-login"}'
    ;;
  *)
    printf '%s' '{"ok":false,"error":"unexpected request"}'
    exit 1
    ;;
esac
EOF
chmod +x "$TMP_DIR/nc"

export PATH="$TMP_DIR:$PATH"
export REQUEST_LOG ROFI_LOG ROFI_RESPONSES

if "$ROOT_DIR/scripts/hwarden-ensure-unlocked"; then
  echo "expected ensure-unlocked to fail when bw requires two-factor setup" >&2
  exit 1
fi

grep -F '"cmd":"unlock"' "$REQUEST_LOG" >/dev/null
unlock_count=$(grep -c '"cmd":"unlock"' "$REQUEST_LOG")
[ "$unlock_count" -eq 1 ]

grep -F "Bitwarden email" "$ROFI_LOG" >/dev/null
grep -F "Bitwarden password" "$ROFI_LOG" >/dev/null
grep -F "two-factor code required; run scripts/hwarden-first-login" "$ROFI_LOG" >/dev/null
if grep -F "Email OTP" "$ROFI_LOG" >/dev/null; then
  echo "frontend should not prompt for Email OTP anymore" >&2
  exit 1
fi
