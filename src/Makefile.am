## Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# enforce compiling this dir first before decending into tests/
SUBDIRS= .

if HAVE_GTEST
SUBDIRS += tests
endif

INCLUDES=$(XAPIAN_CXXFLAGS) $(GMIME_CFLAGS) $(GLIB_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

bin_PROGRAMS=				\
	mu

noinst_LTLIBRARIES=			\
	libmu.la

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mu_SOURCES=				\
	mu.cc

mu_LDADD=				\
	libmu.la

libmu_la_SOURCES=			\
	mu-bookmarks.c			\
	mu-bookmarks.h			\
	mu-cmd-extract.c		\
	mu-cmd-find.c			\
	mu-cmd-index.c			\
	mu-cmd-mkdir.c			\
	mu-cmd-view.c			\
	mu-cmd.c			\
	mu-cmd.h			\
	mu-config.c			\
	mu-config.h			\
	mu-index.c			\
	mu-index.h			\
	mu-log.c			\
	mu-log.h			\
	mu-maildir.c			\
	mu-maildir.h			\
	mu-msg-contact.c		\
	mu-msg-contact.h		\
	mu-msg-fields.c			\
	mu-msg-fields.h			\
	mu-msg-flags.c			\
	mu-msg-flags.h			\
	mu-msg-prio.h			\
	mu-msg-status.h			\
	mu-msg.c			\
	mu-msg.h			\
	mu-msg-priv.h			\
	mu-msg-iter-priv.hh		\
	mu-msg-iter.cc			\
	mu-msg-iter.h			\
	mu-msg-part.c			\
	mu-msg-part.h			\
	mu-msg-str.c			\
	mu-msg-str.h			\
	mu-msg.h			\
	mu-output-link.c		\
	mu-output-link.h		\
	mu-output-plain.c		\
	mu-output-plain.h		\
	mu-query.cc			\
	mu-query.h			\
	mu-result.h			\
	mu-store.cc			\
	mu-store.h			\
	mu-util-db.cc			\
	mu-util-db.h			\
	mu-util.c			\
	mu-util.h

libmu_la_LIBADD=			\
	$(XAPIAN_LIBS)			\
	$(GMIME_LIBS)			\
	$(GLIB_LIBS)
