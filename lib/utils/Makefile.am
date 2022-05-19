## Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	mu-async-queue.hh					\
	mu-command-parser.cc					\
	mu-command-parser.hh					\
	mu-error.hh						\
	mu-logger.cc						\
	mu-logger.hh						\
	mu-option.hh						\
	mu-option.cc						\
	mu-readline.cc						\
	mu-readline.hh						\
	mu-result.hh						\
	mu-sexp.cc						\
	mu-sexp.hh						\
	mu-util.c						\
	mu-util.h						\
	mu-utils.cc						\
	mu-utils.hh						\
	mu-utils-format.hh					\
	mu-xapian-utils.hh

libmu_utils_la_LIBADD=						\
	$(GLIB_LIBS)						\
	$(READLINE_LIBS)					\
	$(CODE_COVERAGE_LIBS)

include $(top_srcdir)/aminclude_static.am
