TESTSUITE = $(srcdir)/testsuite
EXTRA_DIST = testsuite.at build.at ews.at elisp.at protocols.at package.m4 $(TESTSUITE)

$(TESTSUITE): $(srcdir)/testsuite.at build.at ews.at elisp.at protocols.at
	cd $(srcdir) && autom4te --language=autotest -I . -o testsuite testsuite.at

check-local: $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = autom4te
