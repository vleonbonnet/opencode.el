# AGENTS.md - opencode.el

Emacs client for the OpenCode 2 server (`/api/*`, shared background service).

## Rules

- Never evaluate code in the user's running Emacs to test this package. Every test runs in a separate `emacs -Q --batch` process (`make test`); pixel-width fitting tests run in a separate GUI `emacs -Q` (`make test-gui`).
- Never start, stop or restart the user's OpenCode service from a test. Tests replay recorded events from `test/fixtures/v2/` or talk to the fake service in `test/opencode-test-server.el`.
- `make compile` must stay free of warnings, and `make lint` (checkdoc, package-lint) clean.
- The server is authoritative: buffers render what the server reports and keep only UI state locally.

## Layout

See `doc/design.org` for the module map and the reasons behind the main design choices.
