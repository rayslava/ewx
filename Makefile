.POSIX:
EMACS = emacs
CC = gcc
CFLAGS = -Wall -Wextra -Werror -pedantic -std=c2x \
          -Wformat=2 -Wformat-security \
          -Wnull-dereference -Wstack-protector \
          -Wstrict-overflow=3 -Warray-bounds \
          -fstack-protector-strong \
          -D_FORTIFY_SOURCE=2
CPPFLAGS = -I. -DWLR_USE_UNSTABLE
OPTFLAGS = -ggdb
SRCS = ews.c
WAYLAND_PROTOCOLS = $(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER = $(shell pkg-config --variable=wayland_scanner wayland-scanner)
LIBS = \
	 $(shell pkg-config --cflags --libs wlroots) \
	 $(shell pkg-config --cflags --libs wayland-server) \
	 $(shell pkg-config --cflags --libs xkbcommon)

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
	$(CC) $(CFLAGS) $(CPPFLAGS) $(OPTFLAGS) \
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

clean:
	rm -f ewl.elc ewc.elc ews xdg-shell-protocol.h xdg-shell-protocol.c ewp-protocol.h

pre-commit:
	clang-format -style=Google -i *.c *.h

.DEFAULT_GOAL=compile
.PHONY: compile check clean pre-commit
