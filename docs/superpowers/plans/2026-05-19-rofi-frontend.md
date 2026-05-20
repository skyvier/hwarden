# Rofi Frontend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans when applying this plan in a separate implementation session.

**Goal:** Build a minimalist rofi-based frontend and shell helper layer for `hwarden-agent` without changing the backend.

**Architecture:** Add shell scripts under `scripts/` only. Centralize socket IO and JSON handling in `scripts/hwarden-lib`, keep user-facing rofi behavior in `scripts/hwarden-rofi`, and expose a small low-level/high-level helper surface for scripting.

**Tech Stack:** Bash, `rofi`, `jq`, `nc`, `xclip`

---

### Task 1: Add Shared Frontend Library And Low-level Helpers

**Files:**
- Add: `scripts/hwarden-lib`
- Add: `scripts/hwarden-status`
- Add: `scripts/hwarden-unlock`
- Add: `scripts/hwarden-list-items`
- Add: `scripts/hwarden-get-password`

- [ ] Create `scripts/hwarden-lib` with shared helpers for:
  - dependency checks
  - socket path derivation
  - JSON request sending over `nc -N -U`
  - common response parsing with `jq`
  - concise stderr failure reporting
- [ ] Add low-level wrappers that source the library and expose the existing
      backend API without changing it.
- [ ] Make the scripts executable.
- [ ] Run `bash -n` on the new scripts.

### Task 2: Add High-level Unlock And Copy Helpers

**Files:**
- Add: `scripts/hwarden-ensure-unlocked`
- Add: `scripts/hwarden-copy-password`

- [ ] Implement `hwarden-ensure-unlocked`:
  - query status
  - if locked, prompt via rofi for email and password
  - retry once with rofi OTP prompt when backend says
    `"two-factor code required"`
- [ ] Implement `hwarden-copy-password ITEM_ID`:
  - ensure unlock
  - fetch password by item id
  - pipe password to `xclip`
- [ ] Run `bash -n` on the new scripts.

### Task 3: Add Main Rofi Picker Entry Point

**Files:**
- Add: `scripts/hwarden-rofi`

- [ ] Implement the main rofi flow:
  - ensure unlock
  - call `list-items`
  - render `name [username]` choices
  - map selection back to item id
  - copy selected password to clipboard
- [ ] Ensure rofi cancel exits quietly and non-destructively.
- [ ] Run `bash -n` on the script.

### Task 4: Document Frontend Usage

**Files:**
- Modify: `README.md`

- [ ] Add a short frontend section covering:
  - required tools (`rofi`, `jq`, `nc`, `xclip`)
  - the main entrypoint `scripts/hwarden-rofi`
  - the helper commands
  - the email-OTP-only unlock limitation

### Task 5: Verification

**Files:**
- Verify only

- [ ] Run:
  - `bash -n scripts/hwarden-lib scripts/hwarden-status scripts/hwarden-unlock scripts/hwarden-list-items scripts/hwarden-get-password scripts/hwarden-ensure-unlocked scripts/hwarden-copy-password scripts/hwarden-rofi`
- [ ] Run:
  - `/run/current-system/sw/bin/bash -lc 'HOME=/tmp cabal test'`
- [ ] Inspect `git diff --stat` to confirm the backend was not touched.
