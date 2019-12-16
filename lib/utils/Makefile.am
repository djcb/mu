## Copyright (C) 2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AM_CFLAGS=								\
	$(WARN_CFLAGS)							\
	$(GLIB_CFLAGS)							\
	$(ASAN_CFLAGS)							\
	-DMU_TESTMAILDIR=\"${abs_top_srcdir}/lib/tests/testdir\"	\
	-DMU_TESTMAILDIR2=\"${abs_top_srcdir}/lib/tests/testdir2\"	\
	-Wno-format-nonliteral						\
	-Wno-switch-enum						\
	-Wno-deprecated-declarations					\
	-Wno-inline

AM_CXXFLAGS=								\
	$(WARN_CXXFLAGS)						\
	$(GLIB_CFLAGS)							\
	$(ASAN_CXXFLAGS)

noinst_LTLIBRARIES=							\
	libmu-utils.la

libmu_utils_la_SOURCES=							\
	mu-date.c							\
	mu-date.h							\
	mu-log.c							\
	mu-log.h							\
	mu-str.c							\
	mu-str.h							\
	mu-util.c							\
	mu-util.h

libmu_utils_la_LIBADD=							\
	$(GLIB_LIBS)

libmu_utils_la_LDFLAGS=							\
	$(ASAN_LDFLAGS)

noinst_PROGRAMS=							\
	$(TEST_PROGS)

TEST_PROGS+=								\
	test-mu-util
test_mu_util_SOURCES=							\
	test-mu-util.c
test_mu_util_LDADD=							\
	libmu-utils.la

TEST_PROGS+=								\
	test-mu-str
test_mu_str_SOURCES=							\
	test-mu-str.c
test_mu_str_LDADD=							\
	libmu-utils.la
