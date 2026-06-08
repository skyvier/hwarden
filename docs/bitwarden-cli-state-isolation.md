## Bitwarden CLI state isolation

These notes summarize what the Bitwarden documentation says about redirecting CLI state.

### Confirmed from official docs

- The Bitwarden CLI app data directory can be overridden with the `BITWARDENCLI_APPDATA_DIR` environment variable, and it must point to an absolute path.
- The default CLI storage location on Linux is `~/.config/Bitwarden CLI`.
- Bitwarden documents `BITWARDENCLI_APPDATA_DIR` as the mechanism for using multiple CLI accounts/configurations simultaneously.
- The CLI docs describe `BITWARDENCLI_APPDATA_DIR` as pointing to the location of the CLI configuration file, usually `data.json`.

Sources:

- https://bitwarden.com/help/data-storage/
- https://bitwarden.com/help/cli/

### Server settings

- Bitwarden documents configuring the CLI server with `bw config server ...`.
- The docs do not explicitly say, in one sentence, that server settings are stored in the same redirected `data.json`.
- However, the docs strongly imply that they are, because `BITWARDENCLI_APPDATA_DIR` is described as the location of the CLI configuration file and also as the mechanism for separate CLI configurations/accounts.

Source:

- https://bitwarden.com/help/change-client-environment/

### Practical conclusion

Using a dedicated `BITWARDENCLI_APPDATA_DIR` for `hwarden-agent` should isolate:

- CLI config
- local CLI state/data
- very likely server configuration as well

This is the most promising direction for separating the agent's Bitwarden CLI state from the user's normal `bw` usage.
The agent uses an absolute `BITWARDENCLI_APPDATA_DIR` override when one is
provided. Otherwise it uses `$XDG_CONFIG_HOME/hwarden/bitwarden-cli`, with the
normal `$HOME/.config/hwarden/bitwarden-cli` fallback. This lets services place
CLI appdata in a persistent writable service directory while `BW_SESSION`
remains process-local.

### Session handling note

For vault-access commands, Bitwarden also supports passing the session key with `--session` instead of relying on the `BW_SESSION` environment variable.

Source:

- https://bitwarden.com/help/cli/

### Recommendation

- Give the agent its own dedicated `BITWARDENCLI_APPDATA_DIR`.
- Keep that directory separate from the user's default Bitwarden CLI directory.
- Keep the directory in persistent XDG config storage or a persistent service
  state directory rather than `$XDG_RUNTIME_DIR`, so trusted-device state
  survives reboot.
- Consider using `--session` for vault-access commands where practical, even if `BITWARDENCLI_APPDATA_DIR` is the main isolation mechanism.

### Confidence

- `BITWARDENCLI_APPDATA_DIR` redirects CLI app data/config: confirmed.
- It is suitable for multiple independent CLI configurations: confirmed.
- Server settings are redirected through the same location: strong inference from the docs, but not yet directly verified from source code.
