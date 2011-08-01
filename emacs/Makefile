VERSION=$(shell git describe --tags --dirty)
EMACS=emacs
PREFIX=/usr/local
ELS=mu.el mu-common.el mu-view.el mu-find.el 
ELCS=$(ELS:.el=.elc)

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: $(ELCS)

docs: mu.info

install_lisp:
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp

install_docs: docs
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 mu.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/mu.info

install: install_lisp install_docs

clean:
	rm -fr mu.info $(ELCS) 
