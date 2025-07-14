ACLOCAL_AMFLAGS = -I m4

SUBDIRS = ews elisp

EXTRA_DIST = \
    README.org \
    NOTES.org \
    COPYING \
    protocols/ewp.xml

.PHONY: check-elisp lint-elisp elsa-check cppcheck scan-build tidy pre-commit

check-elisp: all
	$(MAKE) -C elisp check

lint-elisp:
	$(MAKE) -C elisp lint

elsa-check:
	$(MAKE) -C elisp elsa-check

cppcheck:
	$(MAKE) -C ews cppcheck

scan-build:
	$(MAKE) -C ews scan-build

tidy:
	$(MAKE) -C ews tidy

pre-commit:
	$(MAKE) -C ews pre-commit

check-local: check-elisp
