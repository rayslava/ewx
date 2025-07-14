ACLOCAL_AMFLAGS = -I m4

SUBDIRS = ews elisp tests

EXTRA_DIST = \
    README.org \
    NOTES.org \
    COPYING \
    protocols/ewp.xml

.PHONY: check-elisp lint-elisp elsa-check cppcheck scan-build tidy pre-commit unit-test

check-elisp: all
	$(MAKE) -C elisp check

lint-elisp:
	$(MAKE) -C elisp lint

elsa-check:
	$(MAKE) -C elisp elsa

cppcheck:
	$(MAKE) -C ews cppcheck

scan-build:
	$(MAKE) -C ews scan-build

tidy:
	$(MAKE) -C ews tidy

pre-commit:
	@echo "Formatting C/C++ files..."
	@find . -type f \( -name '*.c' -o -name '*.h' \) -exec clang-format -style=Google -i {} \;

unit-test:
	$(MAKE) -C tests unit-test

check-local: check-elisp
