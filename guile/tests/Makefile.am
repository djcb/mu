# Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3, or (at your option) any
## later version.
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

AM_CPPFLAGS=$(XAPIAN_CXXFLAGS)					\
	 $(GMIME_CFLAGS)					\
	 $(GLIB_CFLAGS)						\
	-I ${top_srcdir}					\
	-I ${top_srcdir}/lib					\
	-I ${top_srcdir}/lib/tests				\
	-DMU_TESTMAILDIR=\"${top_srcdir}/lib/tests/testdir\"	\
	-DMU_TESTMAILDIR2=\"${top_srcdir}/lib/tests/testdir2\"	\
	-DMU_TESTMAILDIR3=\"${top_srcdir}/lib/tests/testdir3\"	\
	-DMU_PROGRAM=\"${abs_top_builddir}/mu/mu\"		\
	-DMU_GUILE_MODULE_PATH=\"${abs_top_srcdir}/guile/\"     \
	-DMU_GUILE_LIBRARY_PATH=\"${abs_top_builddir}/guile/.libs\" \
	-DABS_CURDIR=\"${abs_builddir}\"			\
	-DABS_SRCDIR=\"${abs_srcdir}\"

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

noinst_PROGRAMS= $(TEST_PROGS)

TEST_PROGS += test-mu-guile
test_mu_guile_SOURCES= test-mu-guile.c dummy.cc
test_mu_guile_LDADD=${top_builddir}/lib/tests/libtestmucommon.la

# we need to use dummy.cc to enforce c++ linking...
BUILT_SOURCES=					\
	dummy.cc
dummy.cc:
	touch dummy.cc

EXTRA_DIST=test-mu-guile.scm
