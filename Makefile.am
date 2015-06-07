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

# this warns about function that have a cyclomatic complexity of > 10,
# which is a sign that it needs some refactoring. requires the pmccabe
# tool. If all is fine, it outputs nothing
cc10:
	@$(PMCCABE) `find . -name '*.c' -o -name '*.cc'`		\
	| grep -v mu-str-normalize.c				\
	| grep -v mu_str_subject_normalize			\
	| grep -v tests						\
	| sort -nr | awk '($$1 > 10)'

# this warns about functions that are over 35 non-comment lines long, which is a
# sign that they need some refactoring. requires the pmccabe tool. if
# all is fine, it outputs nothing
# note, some functions are  exempted from this rule.
line35:
	@$(PMCCABE) -c `find . -name '*.c' -o -name '*.cc'`	\
	 | grep -v mu-str-normalize.c				\
	 | grep -v mu_str_subject_normalize			\
	 | grep -v config_options_group_find			\
	 | grep -v SCM_DEFINE					\
	 | grep -v tests					\
         | awk '($$5 > 35)'

# get all todo/fixme messages
fixme:
	@grep -i 'FIXME\|TODO' `find src -type f`

# check whether we can run make distcheck from the repo version
gitcheck:
	cd `mktemp -d`;				 \
	git clone git://github.com/djcb/mu.git ; \
	cd mu;					 \
	autoreconf -i ;				 \
	./configure ;				 \
	make distcheck

EXTRA_DIST=		\
	TODO		\
	HACKING		\
	gtest.mk	\
	NEWS		\
	NEWS.org	\
	autogen.sh

doc_DATA =			\
	NEWS.org
