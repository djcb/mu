## Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AM_CPPFLAGS=$(GMIME_CFLAGS) $(GLIB_CFLAGS) -I${prefix}/include
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement -pedantic

noinst_PROGRAMS=gmime-test
gmime_test_SOURCES=gmime-test.c
gmime_test_LDADD=$(GMIME_LIBS) $(GLIB_LIBS)


EXTRA_DIST=			\
	mu-completion.zsh	\
	mu-sexp-convert		\
	mu.spec

