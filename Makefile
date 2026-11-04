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

.PHONY: all compile test-compile test test-one test-gui lint contract clean

# OpenCode checkout whose API document `contract' checks requests against;
# its dependencies must be installed (bun install), as ajv comes from there.
OPENCODE ?= $(HOME)/local/source/opencode
CONTRACT_LOG ?= $(or $(TMPDIR),$(TEMP),/tmp)/opencode-el-requests.jsonl

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

# The byte code the tests load, as a package manager installs it,
# compiled again once a source changed, opencode.el's or one of the
# packages it loads, whose macros expand into it; byte code whose source
# is gone is removed.  Warnings are for compile to report.
TEST_DEPENDS = $(foreach dir,$(filter-out -L,$(DEP_PATH)),$(wildcard $(dir)/*.el))
test-compile:
	@for elc in opencode*.elc; do [ ! -f "$$elc" ] || [ -f "$${elc%c}" ] || rm -f "$$elc"; done; \
	stale=; for elc in $(SOURCES:.el=.elc); do [ -f "$$elc" ] || stale=1; done; \
	if [ -z "$$stale" ]; then \
	  oldest=$$(ls -tr $(SOURCES:.el=.elc) | head -1); \
	  for el in $(SOURCES) $(TEST_DEPENDS); do [ "$$el" -nt "$$oldest" ] && stale=1 && break; done; \
	fi; \
	if [ -n "$$stale" ]; then \
	  log=$$(mktemp); \
	  if $(BATCH) -f batch-byte-compile $(SOURCES) > "$$log" 2>&1; then \
	    echo "Compiled $(words $(SOURCES)) files for the tests"; rm -f "$$log"; \
	  else \
	    cat "$$log"; rm -f "$$log"; exit 1; \
	  fi; \
	fi

# Each test file runs alone, in a batch Emacs of its own, so that each
# requires what it uses rather than what files before it loaded, and
# JOBS of them at once, through test/batch-tests.sh: each under TIMEOUT
# seconds and RSS_LIMIT kB of resident memory, its standard input
# closed so that a test reading it fails rather than waits.  A line
# tells each Emacs's count as it ends, then the log of each that failed
# shows.  TEST_SHARDS names the longest files, longest first, and how
# many Emacs share the tests of each, which start first: shares that
# take about as long as one another by the times of the last run.
# SELECTOR, a regexp, runs some tests only.  TEST_CACHE keeps between
# runs those times and the trampolines native compilation makes for
# primitives tests redefine (test/batch-tests.el).
JOBS ?= $(shell n=$$(getconf _NPROCESSORS_ONLN 2> /dev/null || echo 4); [ "$$n" -gt 8 ] && n=8; echo $$n)
TIMEOUT ?= 600
RSS_LIMIT ?= 2097152
TEST_CACHE ?= $(patsubst %/,%,$(or $(TMPDIR),$(TEMP),/tmp))/opencode-el-tests
TEST_SHARDS = opencode-lifecycle-test:3 opencode-connection-test:3 opencode-transport-test:2 opencode-services-test:2
shards = $(or $(word 2,$(subst :, ,$(filter $(1):%,$(TEST_SHARDS)))),1)
SHARDED = $(foreach shard,$(TEST_SHARDS),test/$(word 1,$(subst :, ,$(shard))).el)
TEST_JOBS = $(foreach test,$(filter $(TESTS),$(SHARDED)) $(filter-out $(SHARDED),$(TESTS)),\
  $(foreach n,$(call shards,$(basename $(notdir $(test)))),\
    $(foreach i,$(wordlist 1,$(n),1 2 3 4 5 6 7 8 9),$(test).$(i).$(n))))
TEST_LOGS ?=
NOPIPE = trap '' PIPE;
TEST_ENV = SELECTOR="$(SELECTOR)" BATCH_TESTS_ELN="$(TEST_CACHE)/eln"

test: test-compile
	@logs=$$(mktemp -d "$${TMPDIR:-/tmp}/opencode-el-tests.XXXXXX"); start=$$(date +%s); \
	mkdir "$$logs/times"; cp "$(TEST_CACHE)"/times/*.times "$$logs/times" 2> /dev/null; \
	$(MAKE) --no-print-directory -j$(JOBS) TEST_LOGS="$$logs" $(addprefix test-job/,$(TEST_JOBS)); \
	test/batch-tests.sh report "$$logs" $(JOBS) $$(($$(date +%s) - start)) "$(TEST_CACHE)/times"

# One Emacs of test: test-job/FILE.I.N runs the I-th of N shares of
# the tests of FILE, by the times of the run before this one.
test-job/%:
	@job=$*; n=$${job##*.}; job=$${job%.*}; i=$${job##*.}; file=$${job%.*}; \
	base=$$(basename $$file .el); name=$$base; [ $$n -eq 1 ] || name=$$base.$$i; \
	$(TEST_ENV) BATCH_TESTS_FILE=$$file BATCH_TESTS_SHARD=$$i/$$n \
	BATCH_TESTS_TIMES="$(TEST_LOGS)/times/$$base.times" BATCH_TESTS_TIMES_OUT="$(TEST_LOGS)/$$name.times" \
	test/batch-tests.sh run -t $(TIMEOUT) -m $(RSS_LIMIT) -o "$(TEST_LOGS)/$$name.log" -n $$name \
	  $(BATCH) -l test/opencode-test-helpers.el -l $$file -l test/batch-tests.el -f batch-tests-run

# One test file, FILE, or the tests of it SELECTOR matches: what to run
# while working, before the whole of test.
test-one: test-compile
	@if [ -z "$(FILE)" ]; then echo "test-one needs FILE, a test file"; exit 2; fi
	$(NOPIPE) $(TEST_ENV) BATCH_TESTS_FILE="$(FILE)" \
	$(BATCH) -l test/opencode-test-helpers.el -l $(FILE) -l test/batch-tests.el -f batch-tests-run \
	  < /dev/null

# The tests tagged gui, which measure pixels, in one graphical Emacs.
# It prints only to its *Messages*, written out as it exits.
test-gui: test-compile
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
