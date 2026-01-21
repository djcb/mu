## Copyright (C) 2008-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

# Makefile with some useful targets for meson/ninja
V                  ?= 0

BUILDDIR           ?= $(CURDIR)/build
BUILDDIR_COVERAGE  ?= $(CURDIR)/build-coverage
BUILDDIR_VALGRIND  ?= $(CURDIR)/build-valgrind
BUILDDIR_BENCHMARK ?= $(CURDIR)/build-benchmark

GENHTML            ?= genhtml
LCOV               ?= lcov
MAKEINFO           ?= makeinfo
MESON              ?= meson
NINJA              ?= ninja
VALGRIND           ?= valgrind

ifneq ($(V),0)
  VERBOSE=--verbose
endif

# when MU_HACKER is set, do a debug build MU_HACKER is for djcb & compatible
# developers note that mu requires C++20, we only pass C++23 here for the better
# error messages (esp. for fmt).
ifneq (${MU_HACKER},)
MESON_FLAGS:=$(MESON_FLAGS) '-Dbuildtype=debug'    \
			    '-Dreadline=enabled'   \
			    '-Dguile=enabled'      \
			    '-Dcpp_std=c++23'
endif

.PHONY: all build-valgrind
.PHONY: check test test-verbose-if-fail test-valgrind test-helgrind
.PHONY: benchmark coverage
.PHONY: dist install uninstall clean distclean
.PHONY: mu4e-doc-html

# MESON_FLAGS, e.g. "-Dreadline=enabled"

# examples:
# 1. build with clang, and the thread-sanitizer
#   make clean all MESON_FLAGS="-Db_sanitize=thread" CXX=clang++ CC=clang
all: $(BUILDDIR)
	@$(MESON) compile -C $(BUILDDIR) $(VERBOSE)

setup:
	@$(MESON) setup $(MESON_FLAGS) $(BUILDDIR)
	@ln -sf $(BUILDDIR)/compile_commands.json $(CURDIR) || /bin/true

$(BUILDDIR): setup

check: test

test: all
	@$(MESON) test $(VERBOSE) -C $(BUILDDIR)

install: $(BUILDDIR)
	@$(MESON) install -C $(BUILDDIR) $(VERBOSE)

uninstall: $(BUILDDIR)
	@$(NINJA) -C $(BUILDDIR) uninstall

clean:
	@rm -rf $(BUILDDIR) $(BUILDDIR_COVERAGE) $(BUILDDIR_VALGRIND) $(BUILDDIR_BENCHMARK)
	@rm -rf compile_commands.json

#
# below targets are just for development/testing/debugging. They may or
# may not work on your system.
#
test-verbose-if-fail: all
	$(MESON) test -C $(BUILDDIR) || $(MESON) test -C $(BUILDDIR) --verbose

build-valgrind: $(BUILDDIR_VALGRIND)
	@$(MESON) compile -C $(BUILDDIR_VALGRIND) $(VERBOSE)

$(BUILDDIR_VALGRIND):
	@$(MESON) setup --buildtype=debug $(BUILDDIR_VALGRIND)

vg_opts:=--enable-debuginfod=no --leak-check=full --error-exitcode=1
test-valgrind: export G_SLICE=always-malloc
test-valgrind: export G_DEBUG=gc-friendly
test-valgrind: export MU_VALGRIND=memcheck
test-valgrind: build-valgrind
	@$(MESON) test -C $(BUILDDIR_VALGRIND)			\
		--wrap="$(VALGRIND) $(vg_opts)"			\
		--timeout-multiplier 100

check-valgrind: test-valgrind

# we do _not_ pass helgrind; but this seems to be a false-alarm
#    https://gitlab.gnome.org/GNOME/glib/-/issues/2662
test-helgrind: export MU_VALGRIND=helgrind
test-helgrind: $(BUILDDIR_VALGRIND)
	$(MESON) -C $(BUILDDIR_VALGRIND) test	\
	--wrap="$(VALGRIND) --tool=helgrind --error-exitcode=1"	\
	--timeout-multiplier 100

check-helgrind: test-helgrind

#
# benchmarking
#

$(BUILDDIR_BENCHMARK):
	@$(MESON) setup --buildtype=debugoptimized $(BUILDDIR_BENCHMARK)

build-benchmark-target: $(BUILDDIR_BENCHMARK)
	@$(MESON) compile -C $(BUILDDIR_BENCHMARK) $(VERBOSE)

benchmark: build-benchmark-target
	$(NINJA) -C $(BUILDDIR_BENCHMARK) benchmark

#
# coverage
#

$(BUILDDIR_COVERAGE):
	$(MESON) setup -Db_coverage=true --buildtype=debug $(BUILDDIR_COVERAGE)

covfile:=$(BUILDDIR_COVERAGE)/meson-logs/coverage.info

# generate by hand, meson's built-ins are rather inflexible
coverage: $(BUILDDIR_COVERAGE)
	@$(MESON) compile -C $(BUILDDIR_COVERAGE)
	@$(MESON) test -C $(BUILDDIR_COVERAGE) $(VERBOSE)
	$(LCOV) --capture --directory . --output-file $(covfile)
	@$(LCOV) --remove $(covfile) '/usr/*' '*guile*' '*thirdparty*' '*/tests/*' '*mime-object*' --output $(covfile)
	@$(LCOV) --remove $(covfile) '*mu/mu/*' --output $(covfile)
	@mkdir -p $(BUILDDIR_COVERAGE)/meson-logs/coverage
	@$(GENHTML) $(covfile) --output-directory $(BUILDDIR_COVERAGE)/meson-logs/coverage/
	@echo "coverage report at: file://$(BUILDDIR_COVERAGE)/meson-logs/coverage/index.html"

#
# misc
#

dist: $(BUILDDIR)
	$(MESON) compile -C $(BUILDDIR) $(VERBOSE)
	$(MESON) dist -C $(BUILDDIR) $(VERBOSE)

distclean: clean

#
# documentation
#
BUILDAUX:=./build-aux
DOCPATH=${BUILDDIR}/doc
MU4E_DOCHTML=${DOCPATH}/mu4e

mu4e-doc-html:
	@mkdir -p ${MU4E_DOCHTML} && cp $(BUILDAUX)/texinfo-klare.css ${MU4E_DOCHTML}
	@cd mu4e; makeinfo -v -I ${BUILDDIR} -I ${BUILDAUX} \
		  -I ${BUILDDIR}/mu4e --html --css-ref=texinfo-klare.css -o ${MU4E_DOCHTML} mu4e.texi
mu4e-doc-pdf:
	@mkdir -p ${DOCPATH}
	@cd mu4e; makeinfo -v -I ${BUILDDIR} -I ${BUILDDIR}/mu4e -I ${BUILDAUX} \
		--pdf -o ${DOCPATH}/mu4e.pdf mu4e.texi

MU_SCM_DOCHTML=${DOCPATH}/mu-scm
mu-scm-doc-html:
	@mkdir -p ${MU_SCM_DOCHTML} && cp $(BUILDAUX)/texinfo-klare.css ${MU_SCM_DOCHTML}
	@cd scm; makeinfo -v -I ${BUILDDIR} -I ${BUILDDIR}/scm -I ${BUILDAUX} \
		--html --css-ref=texinfo-klare.css -o ${MU_SCM_DOCHTML} mu-scm.texi
mu-scm-doc-pdf:
	@mkdir -p ${DOCPATH}
	@cd scm; makeinfo -v -I ${BUILDDIR} -I ${BUILDDIR}/scm -I ${BUILDAUX} \
		--pdf -o ${DOCPATH}/mu-scm.pdf mu-scm.texi

doc: mu4e-doc-html mu4e-doc-pdf mu-scm-doc-html mu-scm-doc-pdf
