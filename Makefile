PREFIX  ?= /usr/local
datarootdir ?= $(PREFIX)/share
lispdir ?= $(datarootdir)/emacs/site-lisp/bbdb-vcard
infodir ?= $(datarootdir)/info
docdir  ?= $(datarootdir)/doc/magit
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
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -f batch-byte-compile

VERSION=$(shell test -e .git && git describe --tags --dirty 2> /dev/null)

all: $(ELCS) bbdb-vcard.info

%.elc: %.el
	@$(BATCHC) $<

bbdb-vcard.info: bbdb-vcard.texi
	$(MAKEINFO) $< -o $@

install: install-lisp install-docs

install-lisp: $(ELCS)
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) $(DESTDIR)$(lispdir)

install-docs: bbdb-vcard.info
	$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $< $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/$<
	$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) COPYING $(DESTDIR)$(docdir)

bbdb-vcard-$(VERSION).tar.gz: $(DIST_FILES)
	$(MKDIR) bbdb-vcard-$(VERSION)/bin
	$(CP) $(DIST_FILES) bbdb-vcard-$(VERSION)
	$(CPBIN) $(DIST_FILES_BIN) bbdb-vcard-$(VERSION)/bin
	tar -cvz --mtime=./bbdb-vcard-$(VERSION) -f bbdb-vcard-$(VERSION).tar.gz bbdb-vcard-$(VERSION)
	$(RMDIR) bbdb-vcard-$(VERSION)
