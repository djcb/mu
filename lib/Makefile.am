## Copyright (C) 2010-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# enforce compiling guile (optionally) first,then this dir first
# before descending into tests/
include $(top_srcdir)/gtest.mk

SUBDIRS= utils message index

TESTDEFS=                                               \
        -DMU_TESTMAILDIR=\"${abs_srcdir}/testdir\"      \
        -DMU_TESTMAILDIR2=\"${abs_srcdir}/testdir2\"    \
        -DMU_TESTMAILDIR4=\"${abs_srcdir}/testdir4\"    \
        -DABS_CURDIR=\"${abs_builddir}\"                \
        -DABS_SRCDIR=\"${abs_srcdir}\"


AM_CFLAGS=                                              \
        $(WARN_CFLAGS)                                  \
        $(GMIME_CFLAGS)                                 \
        $(XAPIAN_CFLAGS)                                \
        $(GLIB_CFLAGS)                                  \
        $(GUILE_CFLAGS)                                 \
        $(ASAN_CFLAGS)                                  \
        $(CODE_COVERAGE_CFLAGS)                         \
        $(TESTDEFS)                                     \
        -Wno-format-nonliteral                          \
        -Wno-switch-enum                                \
        -Wno-deprecated-declarations                    \
        -Wno-inline

AM_CXXFLAGS=                                            \
        $(GMIME_CFLAGS)                                 \
        $(GLIB_CFLAGS)                                  \
        $(GUILE_CFLAGS)                                 \
        $(WARN_CXXFLAGS)                                \
        $(XAPIAN_CXXFLAGS)                              \
        $(ASAN_CXXFLAGS)                                \
        $(CODE_COVERAGE_CFLAGS)                         \
        $(TESTDEFS)

AM_CPPFLAGS=                                            \
        $(CODE_COVERAGE_CPPFLAGS)

noinst_LTLIBRARIES=                                     \
        libmu.la

libmu_la_SOURCES=                                       \
        mu-bookmarks.cc                                 \
        mu-bookmarks.hh                                 \
        mu-contacts-cache.cc                            \
        mu-contacts-cache.hh                            \
        mu-data.hh                                      \
        mu-parser.cc                                    \
        mu-parser.hh                                    \
        mu-query.cc                                     \
        mu-query.hh                                     \
        mu-query-results.hh                             \
        mu-query-match-deciders.cc                      \
        mu-query-match-deciders.hh                      \
        mu-query-threads.cc                             \
        mu-query-threads.hh                             \
        mu-runtime.cc                                   \
        mu-runtime.hh                                   \
        mu-script.cc                                    \
        mu-script.hh                                    \
        mu-server.cc                                    \
        mu-server.hh                                    \
        mu-store.cc                                     \
        mu-store.hh                                     \
        mu-tokenizer.cc                                 \
        mu-tokenizer.hh                                 \
        mu-tree.hh                                      \
        mu-xapian.cc                                    \
        mu-xapian.hh                                    \
        mu-maildir.cc                                   \
        mu-maildir.hh                                   \
        mu-msg-crypto.cc                                \
        mu-msg-doc.cc                                   \
        mu-msg-doc.hh                                   \
        mu-msg-file.cc                                  \
        mu-msg-file.hh                                  \
        mu-msg-part.cc                                  \
        mu-msg-part.hh                                  \
	mu-msg-priv.hh                                  \
	mu-msg-sexp.cc                                  \
	mu-msg.cc                                       \
	mu-msg.hh

libmu_la_LIBADD=                                        \
        $(XAPIAN_LIBS)                                  \
        $(GMIME_LIBS)                                   \
        $(GLIB_LIBS)                                    \
        $(GUILE_LIBS)                                   \
        ${builddir}/message/libmu-message.la            \
        ${builddir}/index/libmu-index.la                \
        $(CODE_COVERAGE_LIBS)

libmu_la_LDFLAGS=                                       \
        $(ASAN_LDFLAGS)

noinst_PROGRAMS=                                        \
        tokenize

tokenize_SOURCES=                                       \
        tokenize.cc

tokenize_LDADD=                                         \
        $(WARN_LDFLAGS)                                 \
        libmu.la                                        \
        utils/libmu-utils.la

EXTRA_DIST=                                             \
        doxyfile.in

CLEANFILES=*.log *.trs *core* *vgdump* *.gcda *.gcno

include $(top_srcdir)/aminclude_static.am
