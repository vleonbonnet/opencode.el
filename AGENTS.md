# AGENTS.md - opencode.el

Emacs client for the OpenCode 2 server (`/api/*`, shared background service).

## Rules

- Never evaluate code in the user's running Emacs to test this package. Every test runs in a separate `emacs -Q --batch` process (`make test`); pixel-width fitting tests run in a separate GUI `emacs -Q` (`make test-gui`).
- Never start a graphical Emacs yourself (`Emacs.app/Contents/MacOS/Emacs`, `emacs` without `--batch`, `open -a Emacs`), for tests, experiments or screenshots: on macOS it comes in front of the user's windows and takes the keyboard. Only `make test-gui` starts one, and `dev/bench/run.sh`, both through `test/gui-emacs.sh`: on macOS through `open -g -j -n`, neither activated nor shown, its frames visible to Emacs but off screen, transparent and refusing focus (`test/gui-emacs.el`), under a timeout of 600 s and a 2 GB guard, its status and output returned. An experiment needing a graphical Emacs becomes a test tagged `gui`.
- Test economically. While working, run the test file you change in batch: `make test-one FILE=test/opencode-state-test.el` (`SELECTOR=regexp` for some of its tests), seconds where `make test` takes about 40 s. Run `make compile lint test` once before merging, not after every edit. Run `make test-gui` (about 5 s, one graphical Emacs) only when a change touches what only a graphical frame shows (pixel widths, mode-line rendering), once before merging, never in a loop nor alongside another graphical run; `make test` never starts a graphical Emacs.
- Never start, stop or restart the user's OpenCode service from a test. Tests replay recorded events from `test/fixtures/v2/` or talk to the fake service in `test/opencode-test-server.el`.
- `make compile` must stay free of warnings, and `make lint` (checkdoc, package-lint) clean.
- The server is authoritative: buffers render what the server reports and keep only UI state locally.
- Emacs may follow several services (`doc/design.org`, Several services). State a module keeps for a service is declared with `opencode-defvar-service` and read with `opencode-sv`, the local service's in the variables themselves; every timer, callback, filter and handler of a service's machinery binds its service (`opencode-with-service`, `opencode--timer-arguments`), whatever buffer is current when it runs. The local service must keep behaving, and costing, as with one service.

## Layout

See `doc/design.org` for the module map and the reasons behind the main design choices.
