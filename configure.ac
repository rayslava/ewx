AC_PREREQ([2.71])
AC_INIT([ewx],[0.1.0],[rayslava+ewx@rayslava.com])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])
AC_CONFIG_FILES([
    Makefile
    ews/Makefile
    elisp/Makefile
    tests/Makefile
])

# Configure Autotest
AC_CONFIG_TESTDIR([tests])

AC_PROG_CC
AC_PROG_CC_C_O
AM_PROG_AR
AC_PROG_RANLIB

PKG_PROG_PKG_CONFIG

PKG_CHECK_MODULES([WLROOTS], [wlroots >= 0.15.0])
PKG_CHECK_MODULES([WAYLAND_SERVER], [wayland-server])
PKG_CHECK_MODULES([XKBCOMMON], [xkbcommon])

# Check for Check testing framework (optional)
PKG_CHECK_MODULES([CHECK], [check >= 0.9.4], [have_check=yes], [have_check=no])
AM_CONDITIONAL([HAVE_CHECK], [test "x$have_check" = "xyes"])

# Check for cmocka testing framework (optional)
PKG_CHECK_MODULES([CMOCKA], [cmocka >= 1.1.0], [have_cmocka=yes], [have_cmocka=no])
AM_CONDITIONAL([HAVE_CMOCKA], [test "x$have_cmocka" = "xyes"])

# Check for code coverage tools (optional)
AC_ARG_ENABLE([coverage],
    AS_HELP_STRING([--enable-coverage], [Enable code coverage analysis]),
    [coverage=$enableval],
    [coverage=no])

if test "x$coverage" = "xyes"; then
    AC_CHECK_PROG([LCOV], [lcov], [lcov])
    AC_CHECK_PROG([GENHTML], [genhtml], [genhtml])
    if test "x$LCOV" = "x" -o "x$GENHTML" = "x"; then
        AC_MSG_ERROR([lcov and genhtml are required for coverage analysis. Install lcov package.])
    fi
    COVERAGE_CFLAGS="--coverage -fprofile-arcs -ftest-coverage"
    COVERAGE_LDFLAGS="--coverage"
else
    COVERAGE_CFLAGS=""
    COVERAGE_LDFLAGS=""
fi

AC_SUBST([COVERAGE_CFLAGS])
AC_SUBST([COVERAGE_LDFLAGS])
AM_CONDITIONAL([ENABLE_COVERAGE], [test "x$coverage" = "xyes"])

PKG_CHECK_VAR([WAYLAND_PROTOCOLS_DATADIR], [wayland-protocols], [pkgdatadir])
if test "x$WAYLAND_PROTOCOLS_DATADIR" = "x"; then
    AC_MSG_ERROR([wayland-protocols not found])
fi

PKG_CHECK_VAR([WAYLAND_SCANNER], [wayland-scanner], [wayland_scanner])
if test "x$WAYLAND_SCANNER" = "x"; then
    AC_MSG_ERROR([wayland-scanner not found])
fi

AC_CHECK_PROG([EMACS], [emacs], [emacs])
if test "x$EMACS" = "x"; then
    AC_MSG_ERROR([emacs not found])
fi

AC_ARG_ENABLE([pgtk-check],
    AS_HELP_STRING([--disable-pgtk-check], [Skip checking for Emacs PGTK support (dangerous)]),
    [pgtk_check=$enableval],
    [pgtk_check=yes])

if test "x$pgtk_check" = "xyes"; then
    AC_MSG_CHECKING([if Emacs has PGTK support])
    if $EMACS --batch --eval "(unless (eq window-system 'pgtk) (kill-emacs 1))" 2>/dev/null; then
        AC_MSG_RESULT([yes])
    else
        # Backup check via system-configuration-features
        if $EMACS --batch --eval "(unless (member \"PGTK\" (split-string system-configuration-features)) (kill-emacs 1))" 2>/dev/null; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([Emacs must be built with PGTK support for Wayland compatibility.
Please install emacs-pgtk or rebuild Emacs with --with-pgtk configure option.
Use --disable-pgtk-check to bypass this check.])
        fi
    fi
else
    AC_MSG_NOTICE([Skipping PGTK check as requested])
fi

CFLAGS="$CFLAGS -Wall -Wextra -Wno-error -pedantic -std=c2x"
CFLAGS="$CFLAGS -Wformat=2 -Wformat-security"
CFLAGS="$CFLAGS -Wnull-dereference -Wstack-protector"
CFLAGS="$CFLAGS -Wstrict-overflow=3 -Warray-bounds"
CFLAGS="$CFLAGS -fstack-protector-strong"
CFLAGS="$CFLAGS -D_FORTIFY_SOURCE=2"

AC_ARG_ENABLE([debug],
    AS_HELP_STRING([--enable-debug], [Enable debug build]),
    [debug=$enableval],
    [debug=yes])

if test "x$debug" = "xyes"; then
    CFLAGS="$CFLAGS -ggdb -O1"
else
    CFLAGS="$CFLAGS -Ofast -DNDEBUG -g"
fi

AC_ARG_ENABLE([asan],
    AS_HELP_STRING([--enable-asan], [Enable AddressSanitizer]),
    [asan=$enableval],
    [asan=no])

if test "x$asan" = "xyes"; then
    CFLAGS="$CFLAGS -fsanitize=address -fno-omit-frame-pointer -O1"
    LDFLAGS="$LDFLAGS -fsanitize=address"
fi

AC_ARG_ENABLE([ubsan],
    AS_HELP_STRING([--enable-ubsan], [Enable UndefinedBehaviorSanitizer]),
    [ubsan=$enableval],
    [ubsan=no])

if test "x$ubsan" = "xyes"; then
    CFLAGS="$CFLAGS -fsanitize=undefined -fno-omit-frame-pointer -O1"
    LDFLAGS="$LDFLAGS -fsanitize=undefined"
fi

AC_DEFINE([WLR_USE_UNSTABLE], [1], [Use unstable wlroots API])

AC_OUTPUT
