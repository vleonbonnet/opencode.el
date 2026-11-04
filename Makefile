# Build and test opencode.el in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages (transient and its own),
# one subdirectory each, such as elpaca's builds directory.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
DEP_PATH = $(foreach dep,transient cond-let compat llama,-L $(abspath $(DEPS))/$(dep))
LOAD_PATH = -L . -L test $(DEP_PATH)
# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
BATCH = $(EMACS) -Q --batch $(LOAD_PATH) $(PREFER_NEWER)

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard opencode*.el))
TESTS = $(wildcard test/*-test.el)
# One test file for test-one, and the tests of it to run, a regexp.
FILE ?=
SELECTOR ?=

# A graphical Emacs for test-gui, started by test/gui-emacs.sh: on
# macOS through `open', neither activated nor shown, its frames off
# screen and transparent, so that nothing comes in front of the user's
# windows nor takes the focus; under a timeout and a memory guard, its
# status and output returned.  Never start one otherwise.
GUI_EMACS ?= $(EMACS)
GUI = test/gui-emacs.sh $(GUI_EMACS)

.PHONY: all compile test test-one test-gui lint contract clean

# OpenCode checkout whose API document `contract' checks requests against;
# its dependencies must be installed (bun install), as ajv comes from there.
OPENCODE ?= $(HOME)/local/source/opencode
CONTRACT_LOG ?= $(or $(TMPDIR),$(TEMP),/tmp)/opencode-el-requests.jsonl

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

# One test file, FILE, or the tests of it SELECTOR matches: what to run
# while working, before the whole of test.
test-one:
	@if [ -z "$(FILE)" ]; then echo "test-one needs FILE, a test file"; exit 2; fi
	SELECTOR="$(SELECTOR)" $(BATCH) -l test/opencode-test-helpers.el -l $(FILE) \
	  --eval '(ert-run-tests-batch-and-exit (if (member (getenv "SELECTOR") (quote (nil ""))) t (getenv "SELECTOR")))'

# The tests tagged gui, which measure pixels, in one graphical Emacs.
# It prints only to its *Messages*, written out as it exits.
test-gui:
	$(GUI) $(LOAD_PATH) $(PREFER_NEWER) \
	  --eval '(add-hook (quote kill-emacs-hook) (lambda () (princ (with-current-buffer "*Messages*" (buffer-string)) (function external-debugging-output))))' \
	  -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) \
	  --eval '(kill-emacs (if (zerop (ert-stats-completed-unexpected (ert-run-tests-batch (quote (tag gui))))) 0 1))'

contract:
	$(BATCH) -l dev/contract/record-requests.el "$(CONTRACT_LOG)"
	NODE_PATH="$$(ls -d "$(OPENCODE)"/node_modules/.bun/ajv@8*/node_modules | head -1)" \
	  bun dev/contract/validate.ts "$(OPENCODE)/packages/protocol/openapi.json" "$(CONTRACT_LOG)"

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
