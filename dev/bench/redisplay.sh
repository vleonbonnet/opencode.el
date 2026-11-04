#!/bin/sh
# Time what a session window costs redisplay in a GUI Emacs; see
# dev/bench/redisplay.el.
#
# Usage: dev/bench/redisplay.sh SOURCES OUT
#
# SOURCES is a directory of opencode.el sources to measure, OUT the file
# the results are written to.  BENCH_TURNS (1000) sets the length of the
# transcript.  On macOS, Emacs is an application launched hidden and
# not activated, so that it neither comes to the front nor takes input;
# elsewhere it runs as EMACS.  Either way it runs under a timeout.

set -eu

here=$(cd "$(dirname "$0")/../.." && pwd)
sources=$(cd "$1" && pwd)
out=$2
deps=${DEPS:-$HOME/.emacs.d/elpaca/builds}
emacs=${EMACS:-emacs}
: > "$out"
export BENCH_OUT="$out"

set -- -Q -L "$sources" -L "$here/test" -L "$deps/transient" -L "$deps/cond-let" -L "$deps/compat" -L "$deps/llama" \
  --eval '(push (cons "-ApplePersistenceIgnoreState" (lambda (_) (pop command-line-args-left))) command-switch-alist)' \
  --eval '(setq load-prefer-newer t)' -l "$here/dev/bench/redisplay.el" \
  -ApplePersistenceIgnoreState YES

case "$emacs" in
  */Contents/MacOS/*)
    # A Mac application that quit unexpectedly asks at its next start,
    # in a modal dialog, whether to reopen its windows; the last switch
    # skips that.  The environment reaches the application through
    # --env, as open starts it apart from this shell.
    timeout -k 10 600 open -g -j -n -W -a "${emacs%/Contents/MacOS/*}" \
      --env BENCH_OUT="$out" --env BENCH_TURNS="${BENCH_TURNS:-1000}" --args "$@" || true
    ;;
  *)
    timeout -k 10 600 "$emacs" "$@" || true
    ;;
esac
cat "$out"
