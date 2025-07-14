if HAVE_CHECK
check_PROGRAMS = ews/test_ews
TESTS = ews/test_ews

ews_test_ews_SOURCES = ews/test_ews.c
ews_test_ews_CFLAGS = -std=c2x -Wall -Wextra -pedantic $(CHECK_CFLAGS)
ews_test_ews_LDADD = $(CHECK_LIBS)

.PHONY: unit-test

unit-test:
	@echo "Running C unit tests..."
	./ews/test_ews
endif

TESTSUITE = $(srcdir)/testsuite
EXTRA_DIST = testsuite.at build.at ews.at elisp.at protocols.at package.m4 $(TESTSUITE) ews/test_ews.c

$(TESTSUITE): $(srcdir)/testsuite.at build.at ews.at elisp.at protocols.at
	cd $(srcdir) && autom4te --language=autotest -I . -o testsuite testsuite.at

check-local: $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = autom4te
