## Copyright (C) 2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


################################################################################
# tests
#
test('test-command-parser',
     executable('test-command-parser',
		'test-command-parser.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test-mu-str',
     executable('test-mu-str',
		'test-mu-str.c',
		install: false,
		dependencies: [glib_dep, config_h_dep,lib_mu_utils_dep]))
test('test-mu-util',
     executable('test-mu-util',
		'test-mu-util.c',
		install: false,
		dependencies: [glib_dep,config_h_dep, lib_mu_utils_dep]))
test('test-option',
     executable('test-option',
		'test-option.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test-mu-utils',
     executable('test-mu-utils',
		'test-utils.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test-sexp',
     executable('test-sexp',
		'test-sexp.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep] ))