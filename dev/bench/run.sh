#!/bin/sh
# Measure a reconnection of the event stream in a GUI Emacs.
#
# Usage: dev/bench/run.sh SOURCES OUT
#
# SOURCES is a directory of opencode.el sources to measure, OUT the file
# the results are written to.  The fake service runs in a batch Emacs of
# its own, from this checkout's tests; the measured GUI Emacs talks to
# it alone.  Each Emacs runs under a timeout, and both are killed when
# either grows past 2 GB.
#
# BENCH_FRAMES (22), BENCH_RECONNECTS (3) and BENCH_DEADLINE (400 s)
# shape the run, BENCH_DIRS (53), BENCH_SESSIONS (120) and
# BENCH_MODEL_COPIES (5) the service; BENCH_LOG logs every request on
# both sides, and BENCH_KEEP keeps the scratch directory and its logs.

set -eu

here=$(cd "$(dirname "$0")/../.." && pwd)
sources=$(cd "$1" && pwd)
out=$2
deps=${DEPS:-$HOME/.emacs.d/elpaca/builds}
emacs=${EMACS:-emacs}
root=$(mktemp -d "${TMPDIR:-/tmp}/opencode-bench.XXXXXX")
export BENCH_ROOT="$root" BENCH_OUT="$out"

timeout -k 10 600 "$emacs" -Q --batch -L "$here" -L "$here/test" -L "$deps/plz" -L "$deps/transient" -L "$deps/cond-let" -L "$deps/compat" -L "$deps/llama" \
  -l "$here/dev/bench/service.el" > "$root/service.log" 2>&1 &
service=$!
while [ ! -s "$root/service.json" ]; do
  kill -0 "$service" 2>/dev/null || { cat "$root/service.log"; exit 1; }
  sleep 0.2
done

# A Mac application that quit unexpectedly asks at its next start, in a
# modal dialog, whether to reopen its windows; the switch skips that.
timeout -k 10 600 "$emacs" -Q -L "$sources" -L "$deps/plz" -L "$deps/transient" -L "$deps/cond-let" -L "$deps/compat" -L "$deps/llama" \
  --eval '(push (cons "-ApplePersistenceIgnoreState" (lambda (_) (pop command-line-args-left))) command-switch-alist)' \
  -ApplePersistenceIgnoreState YES \
  --eval '(setq load-prefer-newer t)' -l "$here/dev/bench/client.el" > "$root/client.log" 2>&1 &
client=$!

# Kill both above 2 GB of resident memory.
while kill -0 "$client" 2>/dev/null; do
  for pid in "$service" "$client"; do
    rss=$(ps -o rss= -p "$pid" 2>/dev/null | tr -d ' ' || true)
    if [ -n "$rss" ] && [ "$rss" -gt 2097152 ]; then
      echo "Process $pid exceeded 2 GB; aborting" >&2
      kill -9 "$service" "$client" 2>/dev/null || true
      exit 1
    fi
  done
  sleep 1
done
wait "$client" || true
kill "$service" 2>/dev/null || true
wait "$service" 2>/dev/null || true
cat "$root/client.log"
if [ -n "${BENCH_KEEP:-}" ]; then echo "Kept $root"; else rm -rf "$root"; fi
cat "$out"
