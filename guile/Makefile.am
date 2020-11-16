## Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# note, we need top_builddir for snarfing with 'make distcheck' (ie.,
# with separate builddir)
SUBDIRS= . mu scripts examples tests

AM_CPPFLAGS=						\
	-I. -I${top_builddir} -I${top_srcdir}/lib	\
	${GUILE_CFLAGS}					\
	${GLIB_CFLAGS}

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=						\
	$(ASAN_CFLAGS)					\
	${WARN_CFLAGS}					\
	-Wno-suggest-attribute=noreturn			\
	-Wno-missing-prototypes				\
	-Wno-missing-declarations

AM_CXXFLAGS=						\
	$(ASAN_CXXFLAGS)				\
	${WARN_CXXFLAGS}				\
	-Wno-redundant-decls				\
	-Wno-missing-declarations			\
	-Wno-suggest-attribute=noreturn

lib_LTLIBRARIES=					\
	libguile-mu.la

libguile_mu_la_SOURCES=					\
	mu-guile.cc					\
	mu-guile.hh					\
	mu-guile-message.cc				\
	mu-guile-message.hh

libguile_mu_la_CFLAGS=$(AM_CFLAGS)
libguile_mu_la_CXXFLAGS=$(AM_CXXFLAGS)

libguile_mu_la_LIBADD=					\
	${top_builddir}/lib/libmu.la			\
	$(READLINE_LIBS)				\
	${GUILE_LIBS}

libguile_mu_la_LDFLAGS= 				\
	$(ASAN_LDFLAGS)					\
	-shared						\
	-export-dynamic

XFILES=							\
	mu-guile.x					\
	mu-guile-message.x

info_TEXINFOS=						\
	mu-guile.texi
mu_guile_TEXINFOS=					\
	fdl.texi

BUILT_SOURCES=$(XFILES)

export CPP
snarfcxxopts= $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
SUFFIXES = .x .doc
.cc.x:
	$(AM_V_GEN) $(GUILE_SNARF) -o $@ $< $(snarfcxxopts)


# FIXME: GUILE_SITEDIR would be better, but that
# breaks 'make distcheck'
scmdir=${prefix}/share/guile/site/${GUILE_EFFECTIVE_VERSION}
scm_DATA=mu.scm

EXTRA_DIST=$(scm_DATA)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = $(CC) -M -MG $(snarfcppopts)

CLEANFILES=$(XFILES)
