## Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

NINJA             ?= ninja
BUILDDIR          ?= $(CURDIR)/build
COVERAGE_BUILDDIR ?= $(CURDIR)/build-coverage
MESON             ?= meson
V                 ?= 0

ifneq ($(V),0)
  VERBOSE=--verbose
endif

# when MU_HACKER is set, do a debug build
# MU_HACKER is for djcb & compatible developers
ifneq (${MU_HACKER},)
MESON_FLAGS:=$(MESON_FLAGS) '-Dbuildtype=debug'    \
			    '-Db_sanitize=address' \
			    '-Dreadline=enabled'
endif

.PHONY: all
.PHONY: check test test-verbose-if-fail test-valgrind test-helgrind
.PHONY: benchmark coverage
.PHONY: dist install clean distclean
.PHONY: mu4e-doc-html

# MESON_FLAGS, e.g. "-Dreadline=enabled"

# examples:
# 1. build with clang, and the thread-sanitizer
#   make clean all MESON_FLAGS="-Db_sanitize=thread" CXX=clang++ CC=clang
all: $(BUILDDIR)
	@$(NINJA) -C $(BUILDDIR) $(VERBOSE)
	@ln -sf $(BUILDDIR)/compile_commands.json $(CURDIR) || /bin/true

$(BUILDDIR):
	@$(MESON) setup $(MESON_FLAGS) $(BUILDDIR)

check: test

test: all
	@$(MESON) test $(VERBOSE) -C $(BUILDDIR)


install: $(BUILDDIR)
	@cd $(BUILDDIR); $(MESON) install

clean:
	@rm -rf $(BUILDDIR) $(COVERAGE_BUILDDIR)


#
# below targets are just for development/testing/debugging. They may or
# may not work on your system.
#

test-verbose-if-fail: all
	@cd $(BUILDDIR); $(MESON) test || $(MESON) test --verbose

vg_opts:=--enable-debuginfod=no --leak-check=full --error-exitcode=1
test-valgrind: export G_SLICE=always-malloc
test-valgrind: export G_DEBUG=gc-friendly
test-valgrind: $(BUILDDIR)
	@cd $(BUILDDIR); $(MESON) test		\
		--wrap="valgrind $(vg_opts)"	\
		--timeout-multiplier 100

# we do _not_ pass helgrind; but this seems to be a false-alarm
#    https://gitlab.gnome.org/GNOME/glib/-/issues/2662
# test-helgrind: $(BUILDDIR)
#	@cd $(BUILDDIR); TEST=HELGRIND $(MESON) test			\
#	--wrap='valgrind --tool=helgrind --error-exitcode=1'		\
#	--timeout-multiplier 100

benchmark: $(BUILDDIR)
	$(NINJA) -C $(BUILDDIR) benchmark

$(COVERAGE_BUILDDIR):
	$(MESON) -Db_coverage=true --buildtype=debug $(COVERAGE_BUILDDIR)

covfile:=$(COVERAGE_BUILDDIR)/meson-logs/coverage.info

# generate by hand, meson's built-ins are unflexible
coverage: $(COVERAGE_BUILDDIR)
	$(NINJA) -C $(COVERAGE_BUILDDIR) test
	lcov --capture --directory . --output-file $(covfile)
	@lcov --remove $(covfile) '/usr/*' '*guile*' '*thirdparty*' '*/tests/*' '*mime-object*' --output $(covfile)
	@lcov --remove $(covfile) '*mu/mu/*' --output $(covfile)
	@mkdir -p $(COVERAGE_BUILDDIR)/meson-logs/coverage
	@genhtml $(covfile) --output-directory $(COVERAGE_BUILDDIR)/meson-logs/coverage/
	@echo "coverage report at: file://$(COVERAGE_BUILDDIR)/meson-logs/coverage/index.html"
dist: $(BUILDDIR)
	@cd $(BUILDDIR); $(MESON) dist

distclean: clean

HTMLPATH=${BUILDDIR}/mu4e/mu4e
mu4e-doc-html:
	@mkdir -p ${HTMLPATH} && cp mu4e/texinfo-klare.css ${HTMLPATH}
	@makeinfo -I ${BUILDDIR}/mu4e --html --css-ref=texinfo-klare.css -o ${HTMLPATH} mu4e/mu4e.texi
