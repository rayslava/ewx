.POSIX:
EMACS = emacs
CC = gcc
CFLAGS = -Wall -Wextra -Werror -pedantic -std=c2x \
          -Wformat=2 -Wformat-security \
          -Wnull-dereference -Wstack-protector \
          -Wstrict-overflow=3 -Warray-bounds \
          -fstack-protector-strong \
          -D_FORTIFY_SOURCE=2
INCLUDES = \
	$(shell pkg-config --cflags wlroots) \
	$(shell pkg-config --cflags wayland-server) \
	$(shell pkg-config --cflags xkbcommon)
CPPFLAGS = -I. -DWLR_USE_UNSTABLE $(INCLUDES)
OPTFLAGS = -ggdb
SRCS = ews.c
WAYLAND_PROTOCOLS = $(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER = $(shell pkg-config --variable=wayland_scanner wayland-scanner)
LIBS = \
	 $(shell pkg-config --libs wlroots) \
	 $(shell pkg-config --libs wayland-server) \
	 $(shell pkg-config --libs xkbcommon)

ASAN_FLAGS = -fsanitize=address -fno-omit-frame-pointer -O0
UBSAN_FLAGS = -fsanitize=undefined -fno-omit-frame-pointer -O0

# wayland-scanner is a tool which generates C headers and rigging for Wayland
# protocols, which are specified in XML. wlroots requires you to rig these up
# to your build system yourself and provide them in the include path.
xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

ewp-protocol.h: ewp.xml
	$(WAYLAND_SCANNER) server-header ewp.xml $@

ewp-protocol.c: ewp.xml
	$(WAYLAND_SCANNER) private-code ewp.xml $@

ews: $(SRCS) ewp-protocol.c ewp-protocol.h xdg-shell-protocol.h
	$(CC) $(CFLAGS) $(CPPFLAGS) $(OPTFLAGS) $(AUX_FLAGS) \
		-o $@ $< $(word 2,$^) \
		$(LIBS)

ewc.elc: ewc.el
ewl.elc: ewl.el ewc.elc ewp.xml ews
ewl-test.el: ewl.elc

compile: ewc.elc ewl.elc ews

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

check: ewl-test.el
	$(EMACS) -Q -L . -l $<

cppcheck:
	@echo "Running cppcheck static analysis..."
	cppcheck \
		--enable=all \
		--error-exitcode=1 \
		--std=c23 \
		--check-level=exhaustive \
		--suppress=missingIncludeSystem \
		--suppress=missingInclude \
		-I. \
		$(INCLUDES) \
		-DWLR_USE_UNSTABLE \
		$(SRCS)

# CastToStruct is disabled to use the wl_container_of
scan-build:
	@echo "Running clang static analyzer..."
	$(MAKE) clean
	scan-build \
		--status-bugs \
		-analyze-headers \
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

asan: clean
	$(MAKE) ews AUX_FLAGS="$(ASAN_FLAGS)"

ubsan: clean
	$(MAKE) ews AUX_FLAGS="$(UBSAN_FLAGS)"

ews-release: clean
	$(MAKE) ews AUX_FLAGS="-Ofast -DNDEBUG -g"

clean:
	rm -f ewl.elc ewc.elc ews xdg-shell-protocol.h xdg-shell-protocol.c ewp-protocol.h ewp-protocol.c
	rm -rf clang-analysis

pre-commit:
	clang-format -style=Google -i $(SRCS)

.DEFAULT_GOAL=compile
.PHONY: compile check clean pre-commit cppcheck scan-build ews-asan ews-ubsan ews-msan ews-tsan ews-release sanitized-builds
