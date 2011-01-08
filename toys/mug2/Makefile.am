## Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## t	he Free Software Foundation; either version 3 of the License, or
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

INCLUDES=-I${top_srcdir} -I${top_srcdir}/src $(GTK_CFLAGS) $(WEBKIT_CFLAGS) -DICONDIR='"$(icondir)"' \
         -DGTK_DISABLE_DEPRECATED -DGSEAL_ENABLE

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

noinst_PROGRAMS=					\
	mug2

# note, mug.cc is '.cc' only because libmu must explicitly
# be linked as c++, not c.
mug2_SOURCES=					\
	mug.cc					\
	mug-msg-list-view.c			\
	mug-msg-list-view.h			\
	mug-msg-view.h				\
	mug-msg-view.c				\
	mug-query-bar.h				\
	mug-query-bar.c				\
	mug-shortcuts.c				\
	mug-shortcuts.h

mug2_LDADD=					\
	${top_builddir}/src/libmu.la		\
	${top_builddir}/widgets/libmuwidgets.la	\
	${GTK_LIBS}



