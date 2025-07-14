ELISP_FILES = \
    ewc.el \
    ewl.el

EXTRA_DIST = \
    $(ELISP_FILES) \
    ewc-test.el \
    ewl-test.el

ELCFILES = $(ELISP_FILES:.el=.elc)

SUFFIXES = .el .elc

.el.elc:
	$(EMACS) -Q --batch -L $(srcdir) -L $(top_builddir)/ews -f batch-byte-compile $<

ewl.elc: ewc.elc

all-local: $(ELCFILES)

.PHONY: check-local lint

check-local: ewl-test.el $(ELCFILES)
# TODO(test): Implement a reasonable test for ewl appropriate for CI
#	$(EMACS) -Q -L $(srcdir) -L $(top_builddir)/ews -l $(srcdir)/ewl-test.el

lint: $(ELISP_FILES)
	@echo "Running elisp lint (byte compilation with warnings as errors)..."
	$(EMACS) -Q --batch -L $(srcdir) -L $(top_builddir)/ews \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(srcdir)/ewc.el $(srcdir)/ewl.el

# TODO(test): remove the || true after fixing the issues
elsa: $(ELISP_FILES)
	@echo "Running elsa checks"
	eask lint elsa || true

clean-local:
	rm -f $(ELCFILES)
