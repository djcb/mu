## Copyright (C) 2017-2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	$(GCOV_CFLAGS)		\
	$(ASAN_CXXFLAGS)	\
	-Wno-inline		\
	-Wno-switch-enum

AM_CPPFLAGS=			\
	$(GCOV_CFLAGS)

AM_LDFLAGS=			\
	$(ASAN_LDFLAGS)		\
	$(WARN_LDFLAGS)		\
	$(GCOV_LDADD)

noinst_PROGRAMS=		\
	tokenize		\
	parse

tokenize_SOURCES=		\
	tokenize.cc

tokenize_LDADD=			\
	$(WARN_LDFLAGS)		\
	$(GCOV_LDADD)		\
	libmu-parser.la		\
	../utils/libmu-utils.la

parse_SOURCES=			\
	parse.cc

parse_LDADD=			\
	$(WARN_LDFLAGS)		\
	$(GCOV_LDADD)		\
	libmu-parser.la		\
	../utils/libmu-utils.la

noinst_LTLIBRARIES=		\
	libmu-parser.la

libmu_parser_la_SOURCES=	\
	data.hh			\
	parser.cc		\
	parser.hh		\
	proc-iface.hh		\
	tokenizer.cc		\
	tokenizer.hh		\
	tree.hh			\
	xapian.cc		\
	xapian.hh

libmu_parser_la_LIBADD=		\
	$(WARN_LDFLAGS)		\
	$(GLIB_LIBS)		\
	$(XAPIAN_LIBS)


VALGRIND_SUPPRESSIONS_FILES= 	\
	${top_srcdir}/mu.supp

noinst_PROGRAMS+=$(TEST_PROGS)

TEST_PROGS+= 			\
	test-tokenizer
test_tokenizer_SOURCES=		\
	test-tokenizer.cc
test_tokenizer_LDADD=		\
	$(GCOV_LDADD)		\
	libmu-parser.la		\
	../utils/libmu-utils.la

TEST_PROGS+= 			\
	test-parser
test_parser_SOURCES=		\
	test-parser.cc
test_parser_LDADD=		\
	$(GCOV_LDADD) 		\
	libmu-parser.la		\
	../utils/libmu-utils.la


TESTS=$(TEST_PROGS)
