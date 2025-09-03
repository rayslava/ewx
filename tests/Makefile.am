check_PROGRAMS =
TESTS =

if HAVE_CMOCKA
check_PROGRAMS += ews/test_ews_cmocka ews/test_ews_headless ews/test_ews_input
TESTS += ews/test_ews_cmocka ews/test_ews_headless ews/test_ews_input

AM_CPPFLAGS = -I$(top_builddir)/ews -I$(top_srcdir)/ews $(WLROOTS_CFLAGS) $(WAYLAND_SERVER_CFLAGS) $(XKBCOMMON_CFLAGS) -DWLR_USE_UNSTABLE -DEWS_TESTING
AM_CFLAGS = -std=c2x -Wall -Wextra -pedantic $(CMOCKA_CFLAGS) $(COVERAGE_CFLAGS)
AM_LDFLAGS = $(COVERAGE_LDFLAGS)

EWS_LIBS = $(CMOCKA_LIBS) $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS)
EWS_MODULES = \
    $(top_srcdir)/ews/ews_input.c \
    $(top_srcdir)/ews/ews_cursor.c \
    $(top_srcdir)/ews/ews_output.c \
    $(top_srcdir)/ews/ews_surface.c \
    $(top_srcdir)/ews/ews_ewp.c \
    $(top_srcdir)/ews/ewp-protocol.c

ews_test_ews_cmocka_SOURCES = ews/test_ews_cmocka.c $(top_srcdir)/ews/ews_main.c $(EWS_MODULES)
ews_test_ews_cmocka_LDADD = $(EWS_LIBS)

ews_test_ews_headless_SOURCES = ews/test_ews_headless.c $(EWS_MODULES)
ews_test_ews_headless_LDADD = $(EWS_LIBS)

ews_test_ews_input_SOURCES = ews/test_ews_input.c $(EWS_MODULES)
ews_test_ews_input_LDADD = $(EWS_LIBS)
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
