## Copyright (C) 2010-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AM_CPPFLAGS=							\
	-I${top_srcdir}/lib					\
	$(GLIB_CFLAGS)						\
	$(CODE_COVERAGE_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=							\
	$(JSON_GLIB_CFLAGS)					\
	$(ASAN_CFLAGS)						\
	$(WARN_CFLAGS)						\
	$(CODE_COVERAGE_CFLAGS)					\
	-Wno-switch-enum					\
	-DMU_SCRIPTS_DIR="\"$(pkgdatadir)/scripts/\""

AM_CXXFLAGS=							\
	$(ASAN_CXXCFLAGS)					\
	$(WARN_CXXFLAGS)					\
	$(CODE_COVERAGE_CFLAGS)

AM_LDFLAGS=							\
	$(ASAN_LDFLAGS)

bin_PROGRAMS=							\
	mu

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mu_SOURCES=							\
	mu.cc							\
	mu-cmd-cfind.c						\
	mu-config.c						\
	mu-config.h						\
	mu-cmd-extract.c					\
	mu-cmd-find.c						\
	mu-cmd-index.c						\
	mu-cmd-server.c						\
	mu-cmd-script.c						\
	mu-cmd.c						\
	mu-cmd.h

BUILT_SOURCES=							\
	mu-help-strings.h

mu-help-strings.h: mu-help-strings.txt mu-help-strings.awk
	 $(AM_V_GEN) $(AWK) -f ${top_srcdir}/mu/mu-help-strings.awk < $< > $@

mu_LDADD=							\
	${top_builddir}/lib/libmu.la				\
	$(GLIB_LIBS)						\
	$(CODE_COVERAGE_LIBS)

EXTRA_DIST=							\
	mu-help-strings.awk					\
	mu-help-strings.txt

noinst_PROGRAMS= $(TEST_PROGS)

test_cflags=							\
	${AM_CFLAGS}						\
	-DMU_TESTMAILDIR=\"${abs_top_srcdir}/lib/testdir\"	\
	-DMU_TESTMAILDIR2=\"${abs_top_srcdir}/lib/testdir2\"	\
	-DMU_TESTMAILDIR3=\"${abs_top_srcdir}/lib/testdir3\"	\
	-DMU_TESTMAILDIR4=\"${abs_top_srcdir}/lib/testdir4\"	\
	-DMU_PROGRAM=\"${abs_top_builddir}/mu/mu\"		\
	-DABS_CURDIR=\"${abs_builddir}\"			\
	-DABS_SRCDIR=\"${abs_srcdir}\"

TEST_PROGS += test-mu-query
test_mu_query_SOURCES= test-mu-query.c dummy.cc
test_mu_query_CFLAGS=$(test_cflags)
test_mu_query_LDADD=${top_builddir}/lib/libtestmucommon.la $(CODE_COVERAGE_LIBS)

TEST_PROGS += test-mu-cmd
test_mu_cmd_SOURCES= test-mu-cmd.c dummy.cc
test_mu_cmd_CFLAGS=$(test_cflags)
test_mu_cmd_LDADD=${top_builddir}/lib/libtestmucommon.la $(CODE_COVERAGE_LIBS)

TEST_PROGS += test-mu-cmd-cfind
test_mu_cmd_cfind_SOURCES= test-mu-cmd-cfind.c dummy.cc
test_mu_cmd_cfind_CFLAGS=$(test_cflags)
test_mu_cmd_cfind_LDADD=${top_builddir}/lib/libtestmucommon.la $(CODE_COVERAGE_LIBS)

TEST_PROGS += test-mu-threads
test_mu_threads_SOURCES= test-mu-threads.c dummy.cc
test_mu_threads_CFLAGS=$(test_cflags)
test_mu_threads_LDADD=${top_builddir}/lib/libtestmucommon.la $(CODE_COVERAGE_LIBS)

# we need to use dummy.cc to enforce c++ linking...
BUILT_SOURCES+=							\
	dummy.cc
dummy.cc:
	touch dummy.cc

TESTS=$(TEST_PROGS)
include $(top_srcdir)/aminclude_static.am

CLEANFILES=							\
	$(BUILT_SOURCES)
