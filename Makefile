# Build and test opencode.el in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages (plz, transient),
# one subdirectory each, such as elpaca's builds directory.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
DEP_PATH = $(foreach dep,plz transient cond-let compat llama,-L $(abspath $(DEPS))/$(dep))
LOAD_PATH = -L . -L test $(DEP_PATH)
# A graphical Emacs may start in another directory, such as the Mac
# port's in the home directory, and prints only to its *Messages*.
GUI_LOAD_PATH = -L $(CURDIR) -L $(CURDIR)/test $(DEP_PATH)
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
	$(EMACS) -Q $(GUI_LOAD_PATH) $(PREFER_NEWER) \
	  --eval '(add-hook (quote kill-emacs-hook) (lambda () (princ (with-current-buffer "*Messages*" (buffer-string)) (function external-debugging-output))))' \
	  -l $(CURDIR)/test/opencode-test-helpers.el $(addprefix -l $(CURDIR)/,$(TESTS)) \
	  --eval '(kill-emacs (if (zerop (ert-stats-completed-unexpected (ert-run-tests-batch (quote (tag gui))))) 0 1))'

contract:
	$(BATCH) -l dev/contract/record-requests.el "$(CONTRACT_LOG)"
	NODE_PATH="$$(ls -d "$(OPENCODE)"/node_modules/.bun/ajv@8*/node_modules | head -1)" \
	  bun dev/contract/validate.ts "$(OPENCODE)/packages/protocol/openapi.json" "$(CONTRACT_LOG)"

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

clean:
	rm -f *.elc test/*.elc
