## Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AM_CFLAGS=							\
	$(WARN_CFLAGS)						\
	$(GLIB_CFLAGS)						\
	$(ASAN_CFLAGS)						\
	$(CODE_COVERAGE_CFLAGS)					\
	-DMU_TESTMAILDIR=\"${abs_top_srcdir}/lib/testdir\"	\
	-DMU_TESTMAILDIR2=\"${abs_top_srcdir}/lib/testdir2\"	\
	-Wno-format-nonliteral					\
	-Wno-switch-enum					\
	-Wno-deprecated-declarations				\
	-Wno-inline						\
	-I${top_srcdir}/lib

AM_CPPFLAGS=							\
	$(CODE_COVERAGE_CPPFLAGS)

AM_CXXFLAGS=							\
	$(WARN_CXXFLAGS)					\
	$(GLIB_CFLAGS)						\
	$(ASAN_CXXFLAGS)					\
	$(CODE_COVERAGE_CFLAGS)					\
	-I${top_srcdir}/lib

AM_LDFLAGS=							\
	$(ASAN_LDFLAGS)

noinst_LTLIBRARIES=						\
	libmu-utils.la

libmu_utils_la_SOURCES=						\
	mu-date.c						\
	mu-date.h						\
	mu-error.hh						\
	mu-log.c						\
	mu-log.h						\
	mu-command-parser.cc					\
	mu-command-parser.hh					\
	mu-readline.cc						\
	mu-readline.hh						\
	mu-sexp-parser.cc					\
	mu-sexp-parser.hh					\
	mu-str.c						\
	mu-str.h						\
	mu-util.c						\
	mu-util.h						\
	mu-utils.cc						\
	mu-utils.hh

libmu_utils_la_LIBADD=						\
	$(GLIB_LIBS)						\
	$(READLINE_LIBS)					\
	$(CODE_COVERAGE_LIBS)

noinst_PROGRAMS=						\
	$(TEST_PROGS)

TEST_PROGS+=							\
	test-mu-util
test_mu_util_SOURCES=						\
	test-mu-util.c
test_mu_util_LDADD=						\
	libmu-utils.la

TEST_PROGS+=							\
	test-mu-utils
test_mu_utils_SOURCES=						\
	test-utils.cc
test_mu_utils_LDADD=						\
	libmu-utils.la

TEST_PROGS+=							\
	test-mu-str
test_mu_str_SOURCES=						\
	test-mu-str.c
test_mu_str_LDADD=						\
	libmu-utils.la

TEST_PROGS+=							\
	test-sexp-parser
test_sexp_parser_SOURCES=					\
	test-sexp-parser.cc
test_sexp_parser_LDADD=						\
	libmu-utils.la

TEST_PROGS+=							\
	test-command-parser
test_command_parser_SOURCES=					\
	test-command-parser.cc
test_command_parser_LDADD=					\
	libmu-utils.la

TESTS=$(TEST_PROGS)

include $(top_srcdir)/aminclude_static.am
