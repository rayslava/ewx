# Initialize variables
check_PROGRAMS =
TESTS =

# Temporarily disable old Check tests during CMocka transition
#if HAVE_CHECK
#check_PROGRAMS += ews/test_ews
#TESTS += ews/test_ews
#
#ews_test_ews_SOURCES = ews/test_ews.c
#ews_test_ews_CPPFLAGS = -I$(top_srcdir)/ews $(WLROOTS_CFLAGS) $(WAYLAND_SERVER_CFLAGS) $(XKBCOMMON_CFLAGS) -DWLR_USE_UNSTABLE
#ews_test_ews_CFLAGS = -std=c2x -Wall -Wextra -pedantic $(CHECK_CFLAGS) $(COVERAGE_CFLAGS)
#ews_test_ews_LDADD = $(CHECK_LIBS) $(top_builddir)/ews/ews-ews_utils.o
#ews_test_ews_LDFLAGS = $(COVERAGE_LDFLAGS)
#endif

if HAVE_CMOCKA
check_PROGRAMS += ews/test_ews_cmocka
TESTS += ews/test_ews_cmocka

ews_test_ews_cmocka_SOURCES = ews/test_ews_cmocka.c $(top_srcdir)/ews/ews.c $(top_srcdir)/ews/ewp-protocol.c
ews_test_ews_cmocka_CPPFLAGS = -I$(top_srcdir)/ews -I$(top_builddir)/ews $(WLROOTS_CFLAGS) $(WAYLAND_SERVER_CFLAGS) $(XKBCOMMON_CFLAGS) -DWLR_USE_UNSTABLE -DEWS_TESTING
ews_test_ews_cmocka_CFLAGS = -std=c2x -Wall -Wextra -pedantic $(CMOCKA_CFLAGS) $(COVERAGE_CFLAGS)
ews_test_ews_cmocka_LDADD = $(CMOCKA_LIBS) $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS)
ews_test_ews_cmocka_LDFLAGS = $(COVERAGE_LDFLAGS)
endif

unit-test:
	@echo "Running C unit tests..."
	./ews/test_ews

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
if ENABLE_COVERAGE
	-rm -rf coverage-html
	-rm -f coverage.info coverage-clean.info
	-find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | xargs rm -f
endif

if ENABLE_COVERAGE
.PHONY: unit-test coverage coverage-html coverage-clean coverage-reset

coverage-reset:
	@echo "Resetting coverage counters..."
	$(LCOV) --directory $(top_builddir) --zerocounters

coverage-clean:
	@echo "Cleaning coverage data..."
	-rm -rf coverage-html
	-rm -f coverage.info coverage-clean.info
	-find $(top_builddir) -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | xargs rm -f

coverage: coverage-reset
	@echo "Running tests with coverage analysis..."
	$(MAKE) check
	@echo "Generating coverage report..."
	$(LCOV) --directory $(top_builddir) --capture --output-file coverage.info
	$(LCOV) --remove coverage.info '/usr/*' '*/tests/*' --output-file coverage-clean.info
	$(GENHTML) coverage-clean.info --output-directory coverage-html --title "EWX Code Coverage Report" --legend --show-details
	@echo "Coverage report generated in coverage-html/"
	@echo "Open coverage-html/index.html in a web browser to view the report"

coverage-html: coverage

# Coveralls integration
coverage-coveralls: coverage
	@echo "Preparing coverage data for Coveralls..."
	$(LCOV) --list coverage-clean.info
	@echo "Coverage data ready for Coveralls upload"
	@echo "Use: coveralls --lcov-file coverage-clean.info"

# Generate coverage summary for badges and reports
coverage-summary: coverage
	@echo "Generating coverage summary..."
	@$(LCOV) --summary coverage-clean.info 2>&1 | grep -E "(lines|functions)" | \
		sed 's/.*: \([0-9.]*\)% .*/\1/' > coverage-summary.txt
	@if [ -f coverage-summary.txt ]; then \
		LINE_COV=$$(sed -n '1p' coverage-summary.txt); \
		FUNC_COV=$$(sed -n '2p' coverage-summary.txt); \
		echo "Line Coverage: $$LINE_COV%"; \
		echo "Function Coverage: $$FUNC_COV%"; \
		echo "{\"line_coverage\": $$LINE_COV, \"function_coverage\": $$FUNC_COV}" > coverage.json; \
	fi

endif

AUTOM4TE = autom4te
