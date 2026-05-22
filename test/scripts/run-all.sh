#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

bash "$SCRIPT_DIR/frontend-first-login.sh"
bash "$SCRIPT_DIR/frontend-otp-workflow.sh"
bash "$SCRIPT_DIR/frontend-prefix-filter.sh"
bash "$SCRIPT_DIR/frontend-rofi-progress.sh"
bash "$SCRIPT_DIR/frontend-shell-deps.sh"
