## Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

INCLUDES=$(XAPIAN_CXXFLAGS)					\
	 $(GMIME_CFLAGS)					\
	 $(GLIB_CFLAGS)						\
	-I ${top_srcdir}					\
	-DMU_TESTMAILDIR=\"${abs_srcdir}/testdir\"		\
	-DMU_PROGRAM=\"${abs_top_builddir}/src/mu\"		\
	-DABS_SRCDIR=\"${abs_srcdir}\"

noinst_PROGRAMS= $(TEST_PROGS)

noinst_LTLIBRARIES=libtestmucommon.la

TEST_PROGS += test-mu-util
test_mu_util_SOURCES= test-mu-util.c
test_mu_util_LDADD=   libtestmucommon.la

TEST_PROGS += test-mu-msg-str
test_mu_msg_str_SOURCES= test-mu-msg-str.c
test_mu_msg_str_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-maildir
test_mu_maildir_SOURCES= test-mu-maildir.c
test_mu_maildir_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-query
test_mu_query_SOURCES= test-mu-query.c dummy.cc
test_mu_query_LDADD=    libtestmucommon.la

TEST_PROGS += test-mu-msg
test_mu_msg_SOURCES= test-mu-msg.c
test_mu_msg_LDADD=  libtestmucommon.la


libtestmucommon_la_SOURCES=		\
	test-mu-common.c		\
	test-mu-common.h
libtestmucommon_la_LIBADD= ../libmu.la

# note the question marks; make does not like files with ':', so we
# use the (also supported) version with '!' instead. We could escape
# the : with \: but automake does not recognize that....

# 12 messages, the '.ignore' message should be ignored
# when indexing
EXTRA_DIST=							\
	testdir/tmp/1220863087.12663.ignore			\
	testdir/new/1220863087.12663_9.mindcrime		\
	testdir/new/1220863087.12663_25.mindcrime		\
	testdir/new/1220863087.12663_21.mindcrime		\
	testdir/new/1220863087.12663_23.mindcrime		\
	testdir/cur/1220863087.12663_5.mindcrime!2,S		\
	testdir/cur/1220863087.12663_7.mindcrime!2,RS		\
	testdir/cur/1220863087.12663_15.mindcrime!2,PS		\
	testdir/cur/1220863087.12663_19.mindcrime!2,S		\
	testdir/cur/1220863042.12663_1.mindcrime!2,S		\
	testdir/cur/1220863060.12663_3.mindcrime!2,S		\
	testdir/cur/1283599333.1840_11.cthulhu!2,
