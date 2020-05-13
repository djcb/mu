## Copyright (C) 2008-2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# enforce compiling this dir first before descending into tests/
SUBDIRS= .

AM_CPPFLAGS=-I${top_srcdir} -I${top_srcdir}/lib $(GTK_CFLAGS) $(WEBKIT_CFLAGS)	\
	-DICONDIR='"$(icondir)"' -DMUGDIR='"$(abs_srcdir)"'			\
	-DGSEAL_ENABLE

# remove -DGTK_DISABLE_DEPRECATED for now, since it breaks 3.10+ compilation

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=									\
	$(WARN_CFLAGS)								\
	$(ASAN_CFLAGS)								\
	-Wno-redundant-decls							\
	-Wno-deprecated-declarations						\
	-Wno-switch-enum

AM_CXXFLAGS=$(WARN_CXXFLAGS)

#
# Distributors: this is a _toy_, not for distribution. the "noinst_" says enough
#
noinst_PROGRAMS=								\
	mug

# note, mug.cc is '.cc' only because libmu must explicitly
# be linked as c++, not c.
mug_SOURCES=									\
	mug.c									\
	mug-msg-list-view.c							\
	mug-msg-list-view.h							\
	mug-msg-view.h								\
	mug-msg-view.c								\
	mug-query-bar.h								\
	mug-query-bar.c								\
	mug-shortcuts.c								\
	mug-shortcuts.h								\
	dummy.cc

# we need to use dummy.cc to enforce c++ linking...
BUILT_SOURCES=									\
	dummy.cc

dummy.cc:
	touch dummy.cc

DISTCLEANFILES=									\
	$(BUILT_SOURCES)

mug_LDADD=									\
	$(ASAN_LDFLAGS)								\
	${top_builddir}/lib/libmu.la						\
	libmuwidgets.la								\
	${GTK_LIBS}

noinst_LTLIBRARIES=								\
	libmuwidgets.la

libmuwidgets_la_SOURCES=							\
	mu-widget-util.h							\
	mu-widget-util.c							\
	mu-msg-attach-view.c							\
	mu-msg-attach-view.h							\
	mu-msg-body-view.c							\
	mu-msg-body-view.h							\
	mu-msg-header-view.c							\
	mu-msg-header-view.h							\
	mu-msg-view.h								\
	mu-msg-view.c

libmuwidgets_la_LIBADD=								\
	${top_builddir}/lib/libmu.la						\
	${GTK_LIBS}								\
	${WEBKIT_LIBS}								\
	${GIO_LIBS}

EXTRA_DIST=									\
	mug.svg
