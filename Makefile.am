## Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

if BUILD_GUILE
guile=guile
else
guile=
endif

if BUILD_MU4E
mu4e=mu4e
else
mu4e=
endif

SUBDIRS=m4 man lib $(guile) mu $(mu4e) contrib toys

ACLOCAL_AMFLAGS=-I m4

# so we can say 'make test'
check: test cleanupnote

cleanupnote:
	@echo -e  "\nNote: you can remove the mu-test-<uid> dir in your tempdir"
	@echo "after 'make check' has finished."

tags:
	gtags

EXTRA_DIST=							\
	TODO							\
	HACKING							\
	README							\
	gtest.mk						\
	NEWS							\
	NEWS.org						\
	autogen.sh

doc_DATA =							\
	NEWS.org

include $(top_srcdir)/aminclude_static.am

CODE_COVERAGE_IGNORE_PATTERN=					\
	'/usr/*'						\
	'*test-*'
