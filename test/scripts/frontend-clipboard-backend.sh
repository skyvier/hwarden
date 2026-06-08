#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

cat >"$TMP_DIR/wl-copy" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cat >"$TMP_DIR/wl-copy.txt"
EOF
chmod +x "$TMP_DIR/wl-copy"

cat >"$TMP_DIR/xclip" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >"$TMP_DIR/xclip.args"
cat >"$TMP_DIR/xclip.txt"
EOF
chmod +x "$TMP_DIR/xclip"

export TMP_DIR
export PATH="$TMP_DIR:$PATH"

# shellcheck source=scripts/hwarden-lib
source "$ROOT_DIR/scripts/hwarden-lib"

export WAYLAND_DISPLAY=wayland-1
export DISPLAY=:1
printf '%s' 'wayland-secret' | hwarden_copy_to_clipboard
[ "$(cat "$TMP_DIR/wl-copy.txt")" = "wayland-secret" ]
[ ! -f "$TMP_DIR/xclip.txt" ]

unset WAYLAND_DISPLAY
export DISPLAY=:1
printf '%s' 'x11-secret' | hwarden_copy_to_clipboard
[ "$(cat "$TMP_DIR/xclip.txt")" = "x11-secret" ]
[ "$(cat "$TMP_DIR/xclip.args")" = "-selection clipboard" ]

if (
  unset WAYLAND_DISPLAY DISPLAY
  hwarden_require_clipboard
) 2>"$TMP_DIR/no-display.err"; then
  printf '%s\n' 'expected clipboard dependency check to fail without a display server' >&2
  exit 1
fi

grep -F 'could not detect display server' "$TMP_DIR/no-display.err" >/dev/null
