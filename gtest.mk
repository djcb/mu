## Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

TEST_PROGS=

#
# NOTE: we set the locale/tz to some well-know values, so the tests
# (at least when running under 'make check') run in a predictable
# environment. There are specific tests different timezone, though.
#

test: all $(TEST_PROGS)
	 @export LC_ALL="en_US.utf8"
	 @export TZ="Europe/Helsinki"
	 @test -z "$(TEST_PROGS)" || gtester --verbose $(TEST_PROGS) || exit $$?; \
	 test -z "$(SUBDIRS)" || \
		for subdir in $(SUBDIRS); do \
			test "$$subdir" = "." || \
		(cd ./$$subdir && $(MAKE) $(AM_MAKEFLAGS) $@ ) || exit $$? ; \
		done

.PHONY: test gprof
