# Security specs

This agent runs locally. It exposes a Unix domain socket under $XDG_RUNTIME_DIR. It may receive a Bitwarden username/password over the socket. It unlocks Bitwarden and stores/uses the resulting session key. Only the local user should be able to communicate with the socket. The initial MVP only supports unlocking, listing login items and reading them.

## Explicit non-goals

This does not protect against malware running as the same Unix user.
This does not protect against a compromised Bitwarden CLI.
This does not expose arbitrary bw commands.
This does not accept network connections.
