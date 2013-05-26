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

AM_CPPFLAGS=-I. -I${top_builddir} -I${top_srcdir}/lib ${GUILE_CFLAGS} ${GLIB_CFLAGS}

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

lib_LTLIBRARIES=			\
	libguile-mu.la

libguile_mu_la_SOURCES=			\
	mu-guile.c			\
	mu-guile.h			\
	mu-guile-message.c		\
	mu-guile-message.h

libguile_mu_la_LIBADD=			\
	${top_builddir}/lib/libmu.la	\
	${GUILE_LIBS}

libguile_mu_la_LDFLAGS= -export-dynamic

XFILES=					\
	mu-guile.x			\
	mu-guile-message.x

info_TEXINFOS=				\
	mu-guile.texi
mu_guile_TEXINFOS=			\
	fdl.texi

BUILT_SOURCES=$(XFILES)

snarfcppopts= $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS) $(AM_CPPFLAGS)
SUFFIXES = .x .doc
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

# FIXME: GUILE_SITEDIR would be better, but that
# breaks 'make distcheck'
scmdir=${prefix}/share/guile/site/2.0/
scm_DATA=mu.scm

EXTRA_DIST=$(scm_DATA)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = $(CC) -M -MG $(snarfcppopts)

DISTCLEANFILES=$(XFILES)
