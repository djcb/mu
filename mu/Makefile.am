## Copyright (C) 2010-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AM_CPPFLAGS=                                                    \
	-I${top_srcdir}/lib                                     \
	$(GLIB_CFLAGS)                                          \
	$(XAPIAN_CFLAGS)                                        \
	$(CODE_COVERAGE_CFLAGS)

AM_CXXFLAGS=                                                    \
	$(JSON_GLIB_CFLAGS)                                     \
	-DMU_SCRIPTS_DIR="\"$(pkgdatadir)/scripts/\""
	$(ASAN_CXXCFLAGS)                                       \
	$(WARN_CXXFLAGS)                                        \
	$(CODE_COVERAGE_CFLAGS)                                 \
	-Wno-switch-enum


AM_LDFLAGS=                                                     \
	$(ASAN_LDFLAGS)

bin_PROGRAMS=                                                   \
	mu

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mu_SOURCES=                                                     \
	mu.cc                                                   \
	mu-cmd-cfind.cc                                         \
	mu-config.cc                                            \
	mu-config.hh                                            \
	mu-cmd-extract.cc                                       \
	mu-cmd-find.cc                                          \
	mu-cmd-index.cc                                         \
	mu-cmd-server.cc                                        \
	mu-cmd-script.cc                                        \
	mu-cmd.cc                                               \
	mu-cmd.hh

BUILT_SOURCES=                                                  \
	mu-help-strings.h

mu-help-strings.h: mu-help-strings.txt mu-help-strings.awk
	 $(AM_V_GEN) $(AWK) -f ${top_srcdir}/mu/mu-help-strings.awk < $< > $@

mu_LDADD=                                                       \
	${top_builddir}/lib/libmu.la                            \
	${top_builddir}/lib/utils/libmu-utils.la                \
	$(GLIB_LIBS)                                            \
	$(XAPIAN_LIBS)                                          \
	$(READLINE_LIBS)                                        \
	$(CODE_COVERAGE_LIBS)

EXTRA_DIST=                                                     \
	mu-help-strings.awk                                     \
	mu-help-strings.txt

CLEANFILES=                                                     \
	$(BUILT_SOURCES)
