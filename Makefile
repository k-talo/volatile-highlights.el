EMACS ?= emacs
EMACSFLAGS ?= -Q --batch -L .

EL  := volatile-highlights.el
ELC := $(EL:.el=.elc)
TESTS := $(wildcard test-*.el)
TMPDIR := .tmp

.PHONY: all help compile byte-compile test check clean

all: compile test

help:
	@echo "Targets:"
	@echo "  compile        Byte-compile $(EL)"
	@echo "  test           Run ERT tests (loads $(TESTS))"
	@echo "  clean          Remove .elc and temporary files"
	@echo "Variables:"
	@echo "  EMACS          Emacs binary (current: $(EMACS))"
	@echo "  EMACSFLAGS     Common flags (current: $(EMACSFLAGS))"

compile: $(ELC)
	@:
byte-compile: compile

$(ELC): $(EL)
	$(EMACS) $(EMACSFLAGS) -f batch-byte-compile $<

$(TMPDIR):
	mkdir -p $(TMPDIR)

test: $(TMPDIR)
	$(EMACS) $(EMACSFLAGS) \
	  -eval '(setq temporary-file-directory (expand-file-name "$(TMPDIR)/" default-directory))' \
	  -l $(EL) $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit

check: test

clean:
	rm -f $(ELC)
	rm -rf $(TMPDIR)
