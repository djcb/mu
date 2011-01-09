## Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
INCLUDES=-I${top_srcdir}/src $(GTK_CFLAGS)  $(WEBKIT_CFLAGS) $(GIO_CFLAGS) -DICONDIR='"$(icondir)"' \
         -DGTK_DISABLE_DEPRECATED -DGSEAL_ENABLE

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

noinst_LTLIBRARIES=			\
	libmuwidgets.la

libmuwidgets_la_SOURCES=		\
	mu-widget-util.h		\
	mu-widget-util.c		\
	mu-msg-attach-view.c		\
	mu-msg-attach-view.h		\
	mu-msg-body-view.c		\
	mu-msg-body-view.h		\
	mu-msg-view.h			\
	mu-msg-view.c

libmuwidgets_la_LIBADD=			\
	${top_builddir}/src/libmu.la	\
	${GTK_LIBS}			\
	${WEBKIT_LIBS}			\
	${GIO_LIBS}

