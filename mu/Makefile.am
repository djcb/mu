## Copyright (C) 2010-2016 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

include $(top_srcdir)/gtest.mk

# enforce compiling guile (optionally) first,then this dir first
# before decending into tests/
SUBDIRS= . tests

AM_CPPFLAGS=-I${top_srcdir}/lib $(GLIB_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=						\
	$(JSON_GLIB_CFLAGS)				\
	$(ASAN_CFLAGS)					\
	$(WARN_CFLAGS)					\
	-Wno-switch-enum				\
	-DMU_SCRIPTS_DIR="\"$(pkgdatadir)/scripts/\""

AM_CXXFLAGS=						\
	$(ASAN_CXXCFLAGS)				\
	$(WARN_CXXFLAGS)

AM_LDFLAGS=						\
	$(ASAN_LDFLAGS)

bin_PROGRAMS=						\
	mu

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mu_SOURCES=						\
	mu.cc						\
	mu-cmd-cfind.c					\
	mu-config.c					\
	mu-config.h					\
	mu-cmd-extract.c				\
	mu-cmd-find.c					\
	mu-cmd-index.c					\
	mu-cmd-server.c					\
	mu-cmd-script.c					\
	mu-cmd.c					\
	mu-cmd.h

BUILT_SOURCES=						\
	mu-help-strings.h

mu-help-strings.h: mu-help-strings.txt mu-help-strings.awk
	 $(AM_V_GEN) $(AWK) -f ${top_srcdir}/mu/mu-help-strings.awk < $< > $@

mu_LDADD=						\
	${top_builddir}/lib/libmu.la			\
	$(GLIB_LIBS)

EXTRA_DIST=						\
	mu-help-strings.awk				\
	mu-help-strings.txt

CLEANFILES=$(BUILT_SOURCES)
