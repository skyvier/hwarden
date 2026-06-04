#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

echo "frontend-first-login"
bash "$SCRIPT_DIR/frontend-first-login.sh"
echo "frontend-otp-workflow"
bash "$SCRIPT_DIR/frontend-otp-workflow.sh"
echo "frontend-prefix-filter"
bash "$SCRIPT_DIR/frontend-prefix-filter.sh"
echo "frontend-rofi-progress"
bash "$SCRIPT_DIR/frontend-rofi-progress.sh"
echo "frontend-shell-deps"
bash "$SCRIPT_DIR/frontend-shell-deps.sh"
echo "completed frontend tests"
