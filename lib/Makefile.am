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

# enforce compiling guile (optionally) first,then this dir first
# before decending into tests/
include $(top_srcdir)/gtest.mk

SUBDIRS= utils query

if HAVE_JSON_GLIB
json_srcs=							\
	mu-msg-json.c						\
	mu-msg-json.h
json_flag="-DHAVE_JSON_GLIB"
endif

AM_CFLAGS=							\
	$(WARN_CFLAGS)						\
	$(GMIME_CFLAGS)						\
	$(GLIB_CFLAGS)						\
	$(GUILE_CFLAGS)						\
	$(JSON_GLIB_CFLAGS)					\
	$(ASAN_CFLAGS)						\
	$(json_flag)						\
	$(CODE_COVERAGE_CFLAGS)					\
	-DMU_TESTMAILDIR=\"${abs_srcdir}/testdir\"		\
	-DMU_TESTMAILDIR2=\"${abs_srcdir}/testdir2\"		\
	-DMU_TESTMAILDIR3=\"${abs_srcdir}/testdir3\"		\
	-DMU_TESTMAILDIR4=\"${abs_srcdir}/testdir4\"		\
	-DABS_CURDIR=\"${abs_builddir}\"			\
	-DABS_SRCDIR=\"${abs_srcdir}\"				\
	-Wno-format-nonliteral					\
	-Wno-switch-enum					\
	-Wno-deprecated-declarations				\
	-Wno-inline

AM_CXXFLAGS=							\
	$(GMIME_CFLAGS)						\
	$(GLIB_CFLAGS)						\
	$(GUILE_CFLAGS)						\
	$(JSON_GLIB_CFLAGS)					\
	$(json_flag)						\
	$(WARN_CXXFLAGS)					\
	$(XAPIAN_CXXFLAGS)					\
	$(ASAN_CXXFLAGS)					\
	$(CODE_COVERAGE_CFLAGS)					\
	-DMU_TESTMAILDIR=\"${abs_srcdir}/testdir\"

AM_CPPFLAGS=							\
	$(CODE_COVERAGE_CPPFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
# AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter			\
# -Wdeclaration-after-statement -Wno-variadic-macros
# AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

noinst_LTLIBRARIES=						\
	libmu.la

libmu_la_SOURCES=						\
	mu-bookmarks.c						\
	mu-bookmarks.h						\
	mu-contacts.cc						\
	mu-contacts.hh						\
	mu-container.c						\
	mu-container.h						\
	mu-flags.h						\
	mu-flags.c						\
	mu-index.c						\
	mu-index.h						\
	mu-maildir.c						\
	mu-maildir.h						\
	mu-msg-crypto.c						\
	mu-msg-doc.cc						\
	mu-msg-doc.h						\
	mu-msg-fields.c						\
	mu-msg-fields.h						\
	mu-msg-file.c						\
	mu-msg-file.h						\
	mu-msg-iter.cc						\
	mu-msg-iter.h						\
	$(json_srcs)						\
	mu-msg-part.c						\
	mu-msg-part.h						\
	mu-msg-prio.c						\
	mu-msg-prio.h						\
	mu-msg-priv.h						\
	mu-msg-sexp.c						\
	mu-msg.c						\
	mu-msg.h						\
	mu-msg.h						\
	mu-query.cc						\
	mu-query.h						\
	mu-runtime.cc						\
	mu-runtime.h						\
	mu-script.c						\
	mu-script.h						\
	mu-store.cc						\
	mu-store.hh						\
	mu-threader.c						\
	mu-threader.h

libmu_la_LIBADD=						\
	$(XAPIAN_LIBS)						\
	$(GMIME_LIBS)						\
	$(GLIB_LIBS)						\
	$(GUILE_LIBS)						\
	$(JSON_GLIB_LIBS)					\
	${builddir}/utils/libmu-utils.la			\
	${builddir}/query/libmu-query.la			\
	$(CODE_COVERAGE_LIBS)

libmu_la_LDFLAGS=						\
	$(ASAN_LDFLAGS)

EXTRA_DIST=							\
	mu-msg-crypto.c						\
	doxyfile.in

noinst_PROGRAMS= $(TEST_PROGS)

noinst_LTLIBRARIES+=						\
	libtestmucommon.la

TEST_PROGS += test-mu-maildir
test_mu_maildir_SOURCES= test-mu-maildir.c dummy.cc
test_mu_maildir_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-msg-fields
test_mu_msg_fields_SOURCES= test-mu-msg-fields.c dummy.cc
test_mu_msg_fields_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-msg
test_mu_msg_SOURCES= test-mu-msg.c dummy.cc
test_mu_msg_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-store
test_mu_store_SOURCES= test-mu-store.c dummy.cc
test_mu_store_LDADD= libtestmucommon.la

TEST_PROGS += test-mu-flags
test_mu_flags_SOURCES= test-mu-flags.c dummy.cc
test_mu_flags_LDADD=  libtestmucommon.la

TEST_PROGS += test-mu-container
test_mu_container_SOURCES= test-mu-container.c dummy.cc
test_mu_container_LDADD= libtestmucommon.la

TEST_PROGS += test-mu-contacts
test_mu_contacts_SOURCES= test-mu-contacts.cc
test_mu_contacts_LDADD= libtestmucommon.la

# we need to use dummy.cc to enforce c++ linking...
BUILT_SOURCES=							\
	dummy.cc

dummy.cc:
	touch dummy.cc

libtestmucommon_la_SOURCES=					\
	test-mu-common.c					\
	test-mu-common.h
libtestmucommon_la_LIBADD=					\
	libmu.la

# note the question marks; make does not like files with ':', so we
# use the (also supported) version with '!' instead. We could escape
# the : with \: but automake does not recognize that....

# test messages, the '.ignore' message should be ignored
# when indexing
EXTRA_DIST+=							\
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
	testdir/cur/1283599333.1840_11.cthulhu!2,		\
	testdir/cur/1305664394.2171_402.cthulhu!2,		\
	testdir/cur/1252168370_3.14675.cthulhu!2,S		\
	testdir/cur/encrypted!2,S				\
	testdir/cur/multimime!2,FS				\
	testdir/cur/signed!2,S					\
	testdir/cur/signed-encrypted!2,S			\
	testdir/cur/special!2,Sabc				\
	testdir/cur/multirecip!2,S				\
	testdir2/bar/cur/mail1					\
	testdir2/bar/cur/mail2					\
	testdir2/bar/cur/mail3					\
	testdir2/bar/cur/mail4					\
	testdir2/bar/cur/mail5					\
	testdir2/bar/cur/181736.eml				\
	testdir2/bar/cur/mail6					\
	testdir2/bar/tmp/.noindex				\
	testdir2/bar/new/.noindex				\
	testdir2/Foo/cur/mail5					\
	testdir2/Foo/cur/arto.eml				\
	testdir2/Foo/cur/fraiche.eml				\
	testdir2/Foo/tmp/.noindex				\
	testdir2/Foo/new/.noindex				\
	testdir2/wom_bat/cur/atomic				\
	testdir2/wom_bat/cur/rfc822.1				\
	testdir2/wom_bat/cur/rfc822.2				\
	testdir3/cycle						\
	testdir3/cycle/new/.noindex				\
	testdir3/cycle/cur/rogue0				\
	testdir3/cycle/cur/cycle0				\
	testdir3/cycle/cur/cycle0.0				\
	testdir3/cycle/cur/cycle0.0.0				\
	testdir3/cycle/tmp/.noindex				\
	testdir3/tree/new/.noindex				\
	testdir3/tree/cur/child0.0				\
	testdir3/tree/cur/child4.0				\
	testdir3/tree/cur/root2					\
	testdir3/tree/cur/root1					\
	testdir3/tree/cur/child3.0.0.0.0			\
	testdir3/tree/cur/root0					\
	testdir3/tree/cur/child2.0.0				\
	testdir3/tree/cur/child0.1				\
	testdir3/tree/cur/child0.1.0				\
	testdir3/tree/cur/child4.1				\
	testdir3/tree/tmp/.noindex				\
	testdir3/sort/1st-child-promotes-thread/cur/A		\
	testdir3/sort/1st-child-promotes-thread/cur/B		\
	testdir3/sort/1st-child-promotes-thread/cur/C		\
	testdir3/sort/1st-child-promotes-thread/cur/D		\
	testdir3/sort/2nd-child-promotes-thread/cur/A		\
	testdir3/sort/2nd-child-promotes-thread/cur/B		\
	testdir3/sort/2nd-child-promotes-thread/cur/C		\
	testdir3/sort/2nd-child-promotes-thread/cur/D		\
	testdir3/sort/2nd-child-promotes-thread/cur/E		\
	testdir3/sort/child-does-not-promote-thread/cur/A	\
	testdir3/sort/child-does-not-promote-thread/cur/X	\
	testdir3/sort/child-does-not-promote-thread/cur/Y	\
	testdir3/sort/child-does-not-promote-thread/cur/Z	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/A	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/B	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/C	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/D	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/E	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/F	\
	testdir3/sort/grandchild-promotes-only-subthread/cur/G	\
	testdir3/sort/grandchild-promotes-thread/cur/A		\
	testdir3/sort/grandchild-promotes-thread/cur/B		\
	testdir3/sort/grandchild-promotes-thread/cur/C		\
	testdir3/sort/grandchild-promotes-thread/cur/D		\
	testdir3/sort/grandchild-promotes-thread/cur/E		\
	testdir4/1220863087.12663_19.mindcrime!2,S		\
	testdir4/1220863042.12663_1.mindcrime!2,S		\
	testdir4/1283599333.1840_11.cthulhu!2,			\
	testdir4/1305664394.2171_402.cthulhu!2,			\
	testdir4/1252168370_3.14675.cthulhu!2,S			\
	testdir4/mail1						\
	testdir4/mail5						\
	testdir4/181736.eml					\
	testdir4/encrypted!2,S					\
	testdir4/multimime!2,FS					\
	testdir4/signed!2,S					\
	testdir4/signed-bad!2,S					\
	testdir4/signed-encrypted!2,S				\
	testdir4/special!2,Sabc

TESTS=$(TEST_PROGS)

include $(top_srcdir)/aminclude_static.am
