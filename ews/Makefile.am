bin_PROGRAMS = ews

BUILT_SOURCES = \
    ewp-protocol.h \
    ewp-protocol.c \
    xdg-shell-protocol.h

ewp-protocol.h: $(top_srcdir)/protocols/ewp.xml
	$(WAYLAND_SCANNER) server-header $(top_srcdir)/protocols/ewp.xml $@

ewp-protocol.c: $(top_srcdir)/protocols/ewp.xml
	$(WAYLAND_SCANNER) private-code $(top_srcdir)/protocols/ewp.xml $@

xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS_DATADIR)/stable/xdg-shell/xdg-shell.xml $@

ews_SOURCES = \
    ews.c \
    ewp-protocol.c

nodist_ews_SOURCES = \
    ewp-protocol.h \
    xdg-shell-protocol.h

ews_CPPFLAGS = \
    $(WLROOTS_CFLAGS) \
    $(WAYLAND_SERVER_CFLAGS) \
    $(XKBCOMMON_CFLAGS) \
    -DWLR_USE_UNSTABLE

ews_CFLAGS = $(COVERAGE_CFLAGS)

ews_LDADD = \
    $(WLROOTS_LIBS) \
    $(WAYLAND_SERVER_LIBS) \
    $(XKBCOMMON_LIBS)

ews_LDFLAGS = $(COVERAGE_LDFLAGS)

EXTRA_DIST = $(top_srcdir)/protocols/ewp.xml

CLEANFILES = \
    $(BUILT_SOURCES) \
    xdg-shell-protocol.c

.PHONY: cppcheck scan-build tidy pre-commit

cppcheck:
	@echo "Running cppcheck static analysis..."
	cppcheck \
		--enable=all \
		--error-exitcode=1 \
		--std=c23 \
		--check-level=exhaustive \
		--suppress=missingIncludeSystem \
		--suppress=missingInclude \
		-I$(srcdir) \
		$(WLROOTS_CFLAGS) \
		$(WAYLAND_SERVER_CFLAGS) \
		$(XKBCOMMON_CFLAGS) \
		-DWLR_USE_UNSTABLE \
		$(srcdir)/ews.c

tidy: $(BUILT_SOURCES)
	$(MAKE) clean-local
	bear -- $(MAKE) ews
	clang-tidy $(srcdir)/ews.c

scan-build: clean-local
	@echo "Running clang static analyzer..."
	scan-build \
		--status-bugs \
		-enable-checker security,unix,core,deadcode \
		-disable-checker alpha.core.CastToStruct \
		-o clang-analysis \
		$(MAKE) CC=clang ews
	@if [ -d "clang-analysis" ] && [ "$$(ls -A clang-analysis)" ]; then \
		echo "WARNING: Static analysis found potential issues:"; \
		find clang-analysis -name "*.html" -exec echo "Report: {}" \;; \
		exit 1; \
	else \
		echo "PASSED: No static analysis issues found"; \
	fi

clean-local:
	rm -rf clang-analysis
