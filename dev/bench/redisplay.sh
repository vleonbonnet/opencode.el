#!/bin/sh
# Time what a session window costs redisplay in a GUI Emacs; see
# dev/bench/redisplay.el.
#
# Usage: dev/bench/redisplay.sh SOURCES OUT
#
# SOURCES is a directory of opencode.el sources to measure, OUT the file
# the results are written to.  BENCH_TURNS (1000) sets the length of the
# transcript.  Emacs starts through test/gui-emacs.sh, out of the
# user's sight and focus, under a timeout and a guard of 2 GB.

set -eu

here=$(cd "$(dirname "$0")/../.." && pwd)
sources=$(cd "$1" && pwd)
out=$2
deps=${DEPS:-$HOME/.emacs.d/elpaca/builds}
emacs=${EMACS:-emacs}
: > "$out"
export BENCH_OUT="$out" BENCH_TURNS="${BENCH_TURNS:-1000}"

"$here/test/gui-emacs.sh" -t 600 "$emacs" \
  -L "$sources" -L "$here/test" -L "$deps/transient" -L "$deps/cond-let" -L "$deps/compat" -L "$deps/llama" \
  --eval '(setq load-prefer-newer t)' -l "$here/dev/bench/redisplay.el" || true
cat "$out"
