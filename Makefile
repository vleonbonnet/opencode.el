# Build and test opencode.el in a separate batch Emacs.
#
# DEPS is a directory holding the dependency packages (plz, transient),
# one subdirectory each, such as elpaca's builds directory.

EMACS ?= emacs
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
LOAD_PATH = -L . -L test -L $(DEPS)/plz -L $(DEPS)/transient -L $(DEPS)/cond-let -L $(DEPS)/compat
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard opencode*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile test test-gui lint clean

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

test:
	$(BATCH) -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

test-gui:
	$(EMACS) -Q $(LOAD_PATH) -l test/opencode-test-helpers.el $(addprefix -l ,$(TESTS)) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag gui)))'

lint:
	$(BATCH) --eval '(checkdoc-file "opencode.el")' $(addprefix --eval '(checkdoc-file "',$(addsuffix '")',$(SOURCES)))

clean:
	rm -f *.elc test/*.elc
