## Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# enforce compiling this dir first before decending into tests/
SUBDIRS= .

#if HAVE_GTEST
#SUBDIRS += tests
#endif

INCLUDES=-I${top_srcdir}/src $(GTK_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

bin_PROGRAMS=				\
	mug

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mug_SOURCES=				\
	mug.cc				\
	mug-msg-list-view.c		\
	mug-msg-list-view.h		\
	mug-msg-view.h			\
	mug-msg-view.c			\
	mug-query-bar.h			\
	mug-query-bar.c

mug_LDADD=				\
	${top_srcdir}/src/libmu.la	\
	${GTK_LIBS}

icondir=				\
	$(datarootdir)/icons

icon_DATA=				\
	mug.png

desktopdir=				\
	$(datarootdir)/applications

desktop_DATA=				\
	mug.desktop

EXTRA_DIST=				\
	$(desktopentry_DATA)

