#!/bin/sh
# Measure a reconnection of the event stream in a GUI Emacs, and the
# animated status icons of mode lines (BENCH_RECONNECTS=0 for those alone).
#
# Usage: dev/bench/run.sh SOURCES OUT
#
# SOURCES is a directory of opencode.el sources to measure, OUT the file
# the results are written to.  The fake service runs in a batch Emacs of
# its own, from this checkout's tests; the measured GUI Emacs talks to
# it alone.  Each Emacs runs under a timeout, and both are killed when
# either grows past 2 GB.  The GUI Emacs starts through test/gui-emacs.sh,
# out of the user's sight and focus.
#
# BENCH_FRAMES (22), BENCH_OPEN (26), BENCH_RECONNECTS (3) and
# BENCH_DEADLINE (560 s) shape the run, BENCH_GC_THRESHOLD (100 MiB) the
# client's garbage collections, BENCH_DIRS (53), BENCH_SESSIONS (120),
# BENCH_MODEL_COPIES (5) and BENCH_LONG_TURNS, BENCH_MEDIUM_TURNS and
# BENCH_SHORT_TURNS (400, 60, 8) the service; BENCH_LOG logs every
# request on both sides, and BENCH_KEEP keeps the scratch directory and
# its logs.  For diagnosis, BENCH_PROFILE writes a CPU profile of each
# measure next to OUT, BENCH_TIME counts the longest and total times of
# the functions it names, BENCH_TRACE counts the callers of mode-line
# updates, and BENCH_SLICE sets opencode-replay-slice.

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

# gui-emacs.sh kills the client past 2 GB, and 600 s.
"$here/test/gui-emacs.sh" -t 600 "$emacs" -L "$sources" -L "$deps/plz" -L "$deps/transient" -L "$deps/cond-let" -L "$deps/compat" -L "$deps/llama" \
  --eval '(setq load-prefer-newer t)' -l "$here/dev/bench/client.el" > "$root/client.log" 2>&1 &
client=$!

# Kill both when the service passes 2 GB of resident memory.
while kill -0 "$client" 2>/dev/null; do
  rss=$(ps -o rss= -p "$service" 2>/dev/null | tr -d ' ' || true)
  if [ -n "$rss" ] && [ "$rss" -gt 2097152 ]; then
    echo "Process $service exceeded 2 GB; aborting" >&2
    kill -9 "$service" 2>/dev/null || true
    kill "$client" 2>/dev/null || true
    wait "$client" || true
    exit 1
  fi
  sleep 1
done
wait "$client" || true
kill "$service" 2>/dev/null || true
wait "$service" 2>/dev/null || true
cat "$root/client.log"
if [ -n "${BENCH_KEEP:-}" ]; then echo "Kept $root"; else rm -rf "$root"; fi
cat "$out"
