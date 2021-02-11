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


lib_mu_utils=static_library('mu-utils', [
		  'mu-async-queue.hh',
		  'mu-command-parser.cc',
		  'mu-command-parser.hh',
		  'mu-date.c',
		  'mu-date.h',
		  'mu-error.hh',
		  'mu-logger.cc',
		  'mu-logger.hh',
		  'mu-option.hh',
		  'mu-readline.cc',
		  'mu-readline.hh',
		  'mu-result.hh',
		  'mu-sexp.cc',
		  'mu-sexp.hh',
		  'mu-str.c',
		  'mu-str.h',
		  'mu-util.c',
		  'mu-util.h',
		  'mu-utils.cc',
		  'mu-utils.hh',
		  # third party
		  'optional.hpp',
		  'expected.hpp'],
		 dependencies: [
		   glib_dep,
		   config_h_dep,
		   readline_dep
		 ],
		 include_directories: include_directories(['.','..']),
		 install: false)

lib_mu_utils_dep = declare_dependency(
  link_with: lib_mu_utils,
  include_directories: include_directories(['.', '..'])
)


################################################################################
# tests
#
testmaildir=join_paths(meson.current_source_dir(),'..')

test('test_command_parser',
     executable('test-command-parser',
		'test-command-parser.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test_mu_str',
     executable('test-mu-str',
		'test-mu-str.c',
		install: false,
		dependencies: [glib_dep, config_h_dep,lib_mu_utils_dep]))
test('test_mu_util',
     executable('test-mu-util',
		'test-mu-util.c',
		install: false,
		dependencies: [glib_dep,config_h_dep, lib_mu_utils_dep],
		c_args: ['-DMU_TESTMAILDIR="' + join_paths(testmaildir, 'testdir') + '"',
			 '-DMU_TESTMAILDIR2="' + join_paths(testmaildir, 'testdir2') + '"']))
test('test_option',
     executable('test-option',
		'test-option.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test_mu_utils',
     executable('test-mu-utils',
		'test-utils.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep]))
test('test_sexp',
     executable('test-sexp',
		'test-sexp.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_utils_dep] ))
