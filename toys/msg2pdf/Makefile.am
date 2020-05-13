## Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# enforce compiling this dir first before descending into tests/
SUBDIRS= .

AM_CPPFLAGS=-I${top_srcdir}/lib $(GTK_CFLAGS) $(WEBKIT_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=$(ASAN_CFLAGS) -Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement

noinst_PROGRAMS=					\
	msg2pdf

# note, mug.cc is '.cc' only because libmu must explicitly
# be linked as c++, not c.
msg2pdf_SOURCES=					\
	msg2pdf.c					\
	dummy.cc

# we need to use dummy.cc to enforce c++ linking...
BUILT_SOURCES=						\
	dummy.cc

dummy.cc:
	touch dummy.cc

DISTCLEANFILES=						\
	$(BUILT_SOURCES)


msg2pdf_LDADD=						\
	$(ASAN_LDFLAGS)					\
	${top_builddir}/lib/libmu.la			\
	$(GTK_LIBS)					\
	${WEBKIT_LIBS}
