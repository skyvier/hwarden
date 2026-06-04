#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

nix-shell --pure "$ROOT_DIR/shell.nix" --run '
  set -euo pipefail

  [ "${HWARDEN_BW_PATH:-}" = "$(command -v bw)" ]
  command -v bw >/dev/null
  command -v jq >/dev/null
  command -v nc >/dev/null
  command -v rofi >/dev/null
  command -v xclip >/dev/null
'
