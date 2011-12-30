## Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

# note, we need top_builddir for snarfing with 'make distcheck' (ie.,
# with separate builddir)
SUBDIRS= . mu examples
INCLUDES=-I. -I${top_builddir} -I${top_srcdir}/src ${GUILE_CFLAGS} ${GLIB_CFLAGS}

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get

lib_LTLIBRARIES=			\
	libguile-mu.la

libguile_mu_la_SOURCES=			\
	mu-guile.c			\
	mu-guile.h			\
	mu-guile-msg.c			\
	mu-guile-msg.h			\
	mu-guile-util.c			\
	mu-guile-util.h

libguile_mu_la_LIBADD=			\
	${top_builddir}/src/libmu.la	\
	${GUILE_LIBS}

XFILES=					\
	mu-guile-msg.x			\
	mu-guile.x

moduledir=$(GUILE_SITEDIR)

module_DATA=				\
	mu.scm

BUILT_SOURCES=$(XFILES)

snarfcppopts= $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS) $(INCLUDES)
SUFFIXES = .x .doc
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = $(CC) -M -MG $(snarfcppopts)

DISTCLEANFILES=$(XFILES)

EXTRA_DIST=$(module_DATA)

