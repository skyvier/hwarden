#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

export TMP_DIR
export HWARDEN_SKIP_SOCKET_CHECK=1
export HWARDEN_SOCKET_PATH="$TMP_DIR/agent.sock"

cat > "$TMP_DIR/response.txt" <<'JSON'
{"ok":true,"items":[
  {"id":"1","name":"Demo Mail","username":"demo@example.com"},
  {"id":"null-name","name":null,"username":"null@example.com"},
  {"id":"2","name":"Personal Bank","username":"me@example.com"},
  {"id":"missing-name","username":"missing@example.com"},
  {"id":"3","name":"demo Admin","username":"root@example.com"}
],"cache_age_seconds":125,"cache_refresh_status":"failed"}
JSON

cat > "$TMP_DIR/nc" <<EOF2
#!/usr/bin/env bash
set -euo pipefail

request_json=\$(cat)
printf '%s\n' "\$request_json" >> "$TMP_DIR/requests.log"

cmd=\$(printf '%s' "\$request_json" | jq -r '.cmd // empty')
case "\$cmd" in
  status)
    printf '%s\n' '{"ok":true,"message":"unlocked"}'
    ;;
  list-items)
    cat "$TMP_DIR/response.txt"
    ;;
  get-password)
    item_id=\$(printf '%s' "\$request_json" | jq -r '.id // empty')
    [ "\$item_id" = "1" ] || {
      printf 'unexpected get-password id: %s\n' "\$item_id" >&2
      exit 1
    }
    printf '%s\n' '{"ok":true,"password":"secret"}'
    ;;
  *)
    printf 'unexpected request: %s\n' "\$request_json" >&2
    exit 1
    ;;
esac
EOF2
chmod +x "$TMP_DIR/nc"

cat > "$TMP_DIR/rofi" <<'EOF2'
#!/usr/bin/env bash
set -euo pipefail

printf '%s\n' "$*" >> "$TMP_DIR/rofi-events.log"
for arg in "$@"; do
  if [ "$arg" = "-dmenu" ]; then
    cat > "$TMP_DIR/picker-labels.txt"
    printf '0\n'
    exit 0
  fi
done
EOF2
chmod +x "$TMP_DIR/rofi"

cat > "$TMP_DIR/xclip" <<'EOF2'
#!/usr/bin/env bash
cat > /dev/null
EOF2
chmod +x "$TMP_DIR/xclip"

export PATH="$TMP_DIR:$PATH"
unset WAYLAND_DISPLAY
export DISPLAY=:1

# shellcheck source=scripts/hwarden-lib
source "$REPO_DIR/scripts/hwarden-lib"

items_json=$(jq -c '.items' "$TMP_DIR/response.txt")
expected_filtered='[
  {"id":"1","name":"Demo Mail","username":"demo@example.com"},
  {"id":"3","name":"demo Admin","username":"root@example.com"}
]'
actual_filtered=$(hwarden_filter_items_by_prefix "$items_json" demo)
jq -e -n \
  --argjson actual "$actual_filtered" \
  --argjson expected "$expected_filtered" \
  '$actual == $expected' >/dev/null

run_rofi_picker() {
  local output_file=$1
  shift

  : > "$TMP_DIR/picker-labels.txt"
  : > "$TMP_DIR/rofi-events.log"
  : > "$TMP_DIR/requests.log"

  "$REPO_DIR/scripts/hwarden-rofi" "$@" > /dev/null 2>"$output_file"
}

run_rofi_picker "$TMP_DIR/prefix.stderr" --prefix demo

expected_filtered_labels=$'Demo Mail [demo@example.com]\ndemo Admin [root@example.com]'
expected_unfiltered_labels=$'Demo Mail [demo@example.com]\nnull [null@example.com]\nPersonal Bank [me@example.com]\nnull [missing@example.com]\ndemo Admin [root@example.com]'
actual_labels=$(cat "$TMP_DIR/picker-labels.txt")
grep -F -- "-dmenu -format i -p Bitwarden - cache 2m old, last refresh failed" "$TMP_DIR/rofi-events.log" >/dev/null

[ "$actual_labels" = "$expected_filtered_labels" ] || {
  printf '%s\n' 'expected prefix filtering to restrict picker labels' >&2
  printf 'actual labels were:\n%s\n' "$actual_labels" >&2
  exit 1
}

selected_password_id=$(jq -r '
  select(.cmd == "get-password")
  | .id // empty
' "$TMP_DIR/requests.log")
[ "$selected_password_id" = "1" ] || {
  printf 'expected selected get-password id to be 1, got: %s\n' \
    "$selected_password_id" >&2
  exit 1
}

run_rofi_picker "$TMP_DIR/default.stderr"
actual_labels=$(cat "$TMP_DIR/picker-labels.txt")

[ "$actual_labels" = "$expected_unfiltered_labels" ] || {
  printf '%s\n' 'expected default picker labels to remain unchanged without --prefix' >&2
  printf 'actual labels were:\n%s\n' "$actual_labels" >&2
  exit 1
}

if "$REPO_DIR/scripts/hwarden-rofi" --prefix missing >/dev/null 2>"$TMP_DIR/missing.stderr"; then
  printf '%s\n' 'expected prefix miss invocation to fail' >&2
  exit 1
fi

grep -F -- "-e no login items match prefix: missing" "$TMP_DIR/rofi-events.log" >/dev/null

if "$REPO_DIR/scripts/hwarden-rofi" --prefix >/dev/null 2>"$TMP_DIR/missing-value.stderr"; then
  printf '%s\n' 'expected missing --prefix value invocation to fail' >&2
  exit 1
fi

grep -F "missing value for --prefix" "$TMP_DIR/missing-value.stderr" >/dev/null

if "$REPO_DIR/scripts/hwarden-rofi" --prefix "" >/dev/null 2>"$TMP_DIR/empty-value.stderr"; then
  printf '%s\n' 'expected explicit empty --prefix value invocation to fail' >&2
  exit 1
fi

grep -F "empty value for --prefix" "$TMP_DIR/empty-value.stderr" >/dev/null

if "$REPO_DIR/scripts/hwarden-rofi" --bogus >/dev/null 2>"$TMP_DIR/unknown.stderr"; then
  printf '%s\n' 'expected unknown argument invocation to fail' >&2
  exit 1
fi

grep -F "unknown argument: --bogus" "$TMP_DIR/unknown.stderr" >/dev/null
