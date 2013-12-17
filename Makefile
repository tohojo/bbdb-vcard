PREFIX  ?= /usr/local
datarootdir ?= $(PREFIX)/share
lispdir ?= $(datarootdir)/emacs/site-lisp/bbdb-vcard
infodir ?= $(datarootdir)/info
docdir  ?= $(datarootdir)/doc/bbdb-vcard
execdir ?= $(PREFIX)/bin

ELS  = bbdb-vcard.el vcard.el
ELCS = $(ELS:.el=.elc)

CP    ?= install -p -m 644
CPBIN ?= install -p -m 755
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= install-info

EMACS  ?= $(shell which emacs)
BATCH   = $(EMACS) $(EFLAGS) -batch -L .
BATCHC  = $(BATCH) -f batch-byte-compile

all: $(ELCS) bbdb-vcard.info

%.elc: %.el
	@$(BATCHC) $<

bbdb-vcard.info: bbdb-vcard.texi
	$(MAKEINFO) $< -o $@

install: all install-lisp install-docs

install-lisp: $(ELCS)
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) $(DESTDIR)$(lispdir)

install-docs: bbdb-vcard.info
	$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $< $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/$<
	$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) COPYING README.md $(DESTDIR)$(docdir)

test: $(ELCS) test-bbdb-vcard.el
	$(BATCH) -q -l bbdb -l bbdb-vcard -l test-bbdb-vcard.el --execute '(ert-run-tests-batch-and-exit t)'

clean:
	rm -f *.elc bbdb-vcard.info
