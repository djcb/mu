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

SUBDIRS=src man

# so we can say 'make test'
check: test cleanupnote

cleanupnote:
	@echo -e  "\nNote: you can remove the mu-test-XXXX dirs in your temp-file dir"
	@echo "after 'make check' has finished."

tags:
	gtags

# this warns about function that have a cyclomatic complexity of > 10,
# which is a sign that it needs some refactoring. requires the pmccabe
# tool. If all is fine, it outputs nothing
cc10:
	@pmccabe `find -name '*.c' -o -name '*.cc'` | sort -nr | awk '($$1 > 10)'

# this warns about functions that are over 30 non-comment lines long, which is a
# sign that they need some refactoring. requires the pmccabe tool. if
# all is fine, it outputs nothing
line30:
	@pmccabe -c `find -name '*.c' -o -name '*.cc'` | sort -nr | awk '($$5 > 30)'



EXTRA_DIST=		\
	TODO		\
	HACKING		\
	gtest.mk
