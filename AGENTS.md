# AGENTS.md - opencode.el

Emacs client for the OpenCode 2 server (`/api/*`, a background service shared with other clients). The module map and the reasons behind the design are in `doc/design.org`.

## Commands

- `make test-one FILE=test/opencode-state-test.el` (`SELECTOR=regexp` for some of its tests): the file you change, in seconds. Use it while working.
- `make compile lint test`: once before merging. `compile` must stay free of warnings and `lint` (checkdoc, package-lint) clean; `test` takes about 10 s, each file in a batch Emacs of its own, so no test may count on another having run.
- `make test-gui` (about 5 s, one hidden graphical Emacs) only when a change touches what only a graphical frame shows (pixel widths, mode-line rendering), once before merging.
- `make contract` checks the requests the tests send against the API document of the OpenCode checkout in `~/local/source/opencode` (its dependencies installed with `bun install`): run it when you change a request.

## The user's Emacs

You may evaluate code in the user's running Emacs through Anvil, to inspect state or to try a change; follow `~/.emacs.d/AGENTS.md` (no buffers displayed, no prompts).

- This Mac installs opencode.el from this checkout. After merging to main: `(val/elpaca/update-packages 'opencode :rebuild t)` with `emacs-eval-async`.
- Core, the Windows host, installs it from GitHub: once main is pushed, `myssh core "/ucrt64/bin/emacsclient -f ~/.emacs.d/server/server --eval \"(val/elpaca/update-packages 'opencode)\""`.
- Both run in the background. The outcome, or the failure: `(list (car (gethash (val/elpaca/package-key 'opencode) val/elpaca/stories)) (val/maintenance/failure 'update (val/elpaca/package-key 'opencode)))`.

## Boundaries

- Never start a graphical Emacs yourself (`emacs` without `--batch`, `Emacs.app`, `open -a Emacs`): on macOS it takes the user's screen and keyboard. Only `make test-gui` and the benchmarks `dev/bench/run.sh` and `dev/bench/redisplay.sh` start one, hidden, through `test/gui-emacs.sh`; an experiment needing a graphical frame becomes a test tagged `gui`.
- Never start, stop or restart the user's OpenCode service. Tests replay recorded events from `test/fixtures/v2/` or talk to the fake service of `test/opencode-test-server.el`.

## Conventions the code does not show

- The server is authoritative: buffers render what it reports and keep only interface state.
- Emacs may follow several services (`doc/design.org`, Several services). State a module keeps for a service is declared with `opencode-defvar-service` and read with `opencode-sv`; every timer, callback, filter and handler binds its service (`opencode-with-service`, `opencode--timer-arguments`), whatever buffer is current when it runs. The local service must behave, and cost, as with one service.

## Finishing

Commit messages: an imperative subject, then prose paragraphs saying what changed and why. Merge onto the local main, run `make compile lint test` there, push main, then update opencode.el on this Mac and on Core as above.
