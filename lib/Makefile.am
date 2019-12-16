## Copyright (C) 2010-2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
SUBDIRS= utils parser . tests

if HAVE_JSON_GLIB
json_srcs=					\
	mu-msg-json.c				\
	mu-msg-json.h
json_flag="-DHAVE_JSON_GLIB"
endif

AM_CFLAGS=					\
	$(WARN_CFLAGS)				\
	$(GMIME_CFLAGS)				\
	$(GLIB_CFLAGS)				\
	$(GUILE_CFLAGS)				\
	$(JSON_GLIB_CFLAGS)			\
	$(ASAN_CFLAGS)				\
	$(json_flag)				\
	-Wno-format-nonliteral			\
	-Wno-switch-enum			\
	-Wno-deprecated-declarations		\
	-Wno-inline

AM_CXXFLAGS=					\
	$(WARN_CXXFLAGS)			\
	$(GMIME_CFLAGS)				\
	$(GLIB_CFLAGS)				\
	$(JSON_GLIB_CFLAGS)			\
	$(XAPIAN_CXXFLAGS)			\
	$(GUILE_CFLAGS)				\
	$(ASAN_CXXFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
# AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter	\
# -Wdeclaration-after-statement -Wno-variadic-macros
# AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

noinst_LTLIBRARIES=				\
	libmu.la

libmu_la_SOURCES=				\
	mu-bookmarks.c				\
	mu-bookmarks.h				\
	mu-contacts.cc				\
	mu-contacts.hh				\
	mu-container.c				\
	mu-container.h				\
	mu-flags.h				\
	mu-flags.c				\
	mu-index.c				\
	mu-index.h				\
	mu-maildir.c				\
	mu-maildir.h				\
	mu-msg-crypto.c				\
	mu-msg-doc.cc				\
	mu-msg-doc.h				\
	mu-msg-fields.c				\
	mu-msg-fields.h				\
	mu-msg-file.c				\
	mu-msg-file.h				\
	mu-msg-iter.cc				\
	mu-msg-iter.h				\
	$(json_srcs)				\
	mu-msg-part.c				\
	mu-msg-part.h				\
	mu-msg-prio.c				\
	mu-msg-prio.h				\
	mu-msg-priv.h				\
	mu-msg-sexp.c				\
	mu-msg.c				\
	mu-msg.h				\
	mu-msg.h				\
	mu-query.cc				\
	mu-query.h				\
	mu-runtime.cc				\
	mu-runtime.h				\
	mu-script.c				\
	mu-script.h				\
	mu-store.cc				\
	mu-store.hh				\
	mu-threader.c				\
	mu-threader.h

libmu_la_LIBADD=				\
	$(XAPIAN_LIBS)				\
	$(GMIME_LIBS)				\
	$(GLIB_LIBS)				\
	$(GUILE_LIBS)				\
	$(JSON_GLIB_LIBS)			\
	${builddir}/utils/libmu-utils.la	\
	${builddir}/parser/libmu-parser.la

libmu_la_LDFLAGS=				\
	$(ASAN_LDFLAGS)

EXTRA_DIST=					\
	mu-msg-crypto.c				\
	doxyfile.in
