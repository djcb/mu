EMACS=emacs
PREFIX=/usr/local
ELS=mua.el mua-common.el mua-view.el mua-hdrs.el mua-msg.el
ELCS=$(ELS:.el=.elc)

.PHONY=install

top_srcdir=/home/djcb/src/mu/


BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: $(ELCS)

BUILT_SOURCES=mu-errors.el

mu-errors.el: ${top_srcdir}/src/mu-util.h
	@cat ${top_srcdir}/src/mu-util.h \




docs: mua.info

install_lisp:
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp

install_docs: docs
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 mu.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/mu.info

install: install_lisp install_docs

clean:
	rm -fr mua.info $(ELCS)
