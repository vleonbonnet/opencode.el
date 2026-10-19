# Build and test opencode.el in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages (plz, transient),
# one subdirectory each, such as elpaca's builds directory.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
LOAD_PATH = -L . -L test -L $(DEPS)/plz -L $(DEPS)/transient -L $(DEPS)/cond-let -L $(DEPS)/compat -L $(DEPS)/llama
# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
BATCH = $(EMACS) -Q --batch $(LOAD_PATH) $(PREFER_NEWER)

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard opencode*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile test test-gui lint contract clean

# OpenCode checkout whose API document `contract' checks requests against;
# its dependencies must be installed (bun install), as ajv comes from there.
OPENCODE ?= $(HOME)/local/source/opencode
CONTRACT_LOG ?= $(or $(TMPDIR),$(TEMP),/tmp)/opencode-el-requests.jsonl

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

test-gui:
	$(EMACS) -Q $(LOAD_PATH) $(PREFER_NEWER) -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag gui)))'

contract:
	$(BATCH) -l dev/contract/record-requests.el "$(CONTRACT_LOG)"
	NODE_PATH="$$(ls -d "$(OPENCODE)"/node_modules/.bun/ajv@8*/node_modules | head -1)" \
	  bun dev/contract/validate.ts "$(OPENCODE)/packages/protocol/openapi.json" "$(CONTRACT_LOG)"

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
