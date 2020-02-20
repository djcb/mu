## Copyright (C) 2017-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

@VALGRIND_CHECK_RULES@

AM_CXXFLAGS=			\
	-I$(srcdir)/..		\
	-I$(top_srcdir)/lib	\
	$(GLIB_CFLAGS)		\
	$(XAPIAN_CXXFLAGS)	\
	$(WARN_CXXFLAGS)	\
	$(ASAN_CXXFLAGS)	\
	$(CODE_COVERAGE_CFLAGS)	\
	-Wno-inline		\
	-Wno-switch-enum

AM_CPPFLAGS=			\
	$(CODE_COVERAGE_CPPFLAGS)

AM_LDFLAGS=			\
	$(ASAN_LDFLAGS)		\
	$(WARN_LDFLAGS)

noinst_PROGRAMS=		\
	tokenize		\
	parse

noinst_LTLIBRARIES=		\
	libmu-query.la

libmu_query_la_SOURCES=   	\
	mu-data.hh		\
	mu-parser.cc		\
	mu-parser.hh		\
	mu-proc-iface.hh	\
	mu-tokenizer.cc		\
	mu-tokenizer.hh		\
	mu-tree.hh		\
	mu-xapian.cc		\
	mu-xapian.hh

libmu_query_la_LIBADD=		\
	$(WARN_LDFLAGS)		\
	$(GLIB_LIBS)		\
	$(XAPIAN_LIBS)		\
	../utils/libmu-utils.la \
	$(CODE_COVERAGE_LIBS)

VALGRIND_SUPPRESSIONS_FILES= 	\
	${top_srcdir}/mu.supp

tokenize_SOURCES=		\
	tokenize.cc

tokenize_LDADD=			\
	$(WARN_LDFLAGS)		\
	libmu-query.la

parse_SOURCES=			\
	parse.cc

parse_LDADD=			\
	$(WARN_LDFLAGS)		\
	libmu-query.la

noinst_PROGRAMS+=$(TEST_PROGS)

TEST_PROGS+= 			\
	test-tokenizer
test_tokenizer_SOURCES=		\
	test-tokenizer.cc
test_tokenizer_LDADD=		\
	libmu-query.la

TEST_PROGS+= 			\
	test-parser
test_parser_SOURCES=		\
	test-parser.cc
test_parser_LDADD=		\
	libmu-query.la

TESTS=$(TEST_PROGS)

include $(top_srcdir)/aminclude_static.am
