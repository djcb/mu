## Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#
# tests
#

# BROKEN on ubuntu CI, but works elsewhere. Investigate
test('test-cmd',
     executable('test-cmd',
		'test-mu-cmd.cc',
		install: false,
		dependencies: [glib_dep, config_h_dep, lib_mu_dep]))

test('test-cmd-cfind',
     executable('test-cmd-cfind',
		'test-mu-cmd-cfind.cc',
		install: false,
		dependencies: [glib_dep, config_h_dep, lib_mu_dep]))
test('test-cmd-query',
     executable('test-cmd-query',
		'test-mu-query.cc',
		install: false,
		dependencies: [glib_dep, config_h_dep, lib_mu_dep]))

gmime_test = executable(
  'gmime-test', [
  'gmime-test.c'
],
  dependencies: [ glib_dep, gmime_dep ],
  install: false)
