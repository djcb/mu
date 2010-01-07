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

INCLUDES=$(XAPIAN_CXXFLAGS) $(GMIME_CFLAGS) $(GLIB_CFLAGS)

bin_PROGRAMS=			\
	mu

mu_SOURCES=			\
	mu-cmd.h		\
	mu-cmd.c		\
	mu-config.h		\
	mu-config.c		\
	mu-index.c		\
	mu-index.h		\
	mu-log.c		\
	mu-log.h		\
	mu-maildir.c		\
	mu-maildir.h		\
	mu-msg-fields.c		\
	mu-msg-fields.h		\
	mu-msg-flags.c		\
	mu-msg-flags.h		\
	mu-msg-gmime.c		\
	mu-msg-gmime.h		\
	mu-msg-str.c		\
	mu-msg-str.h		\
	mu-msg-xapian-priv.hh	\
	mu-msg-xapian.cc	\
	mu-msg-xapian.h		\
	mu-query-xapian.cc	\
	mu-query-xapian.h	\
	mu-result.h		\
	mu-store-xapian.cc	\
	mu-store-xapian.h	\
	mu-util.h		\
	mu-util.c		\
	mu.c

mu_LDADD=			\
	$(XAPIAN_LIBS)		\
	$(GMIME_LIBS)		\
	$(GLIB_LIBS)
