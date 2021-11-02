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


awk_script=join_paths(meson.current_source_dir(), 'mu-help-strings.awk')
mu_help_strings_h=custom_target('mu_help',
				input: 'mu-help-strings.txt',
				output: 'mu-help-strings.h',
				command: [awk, '-f', awk_script, '@INPUT@'],
				capture: true)
mu = executable(
  'mu', [
  'mu.cc',
  'mu-cmd-cfind.cc',
  'mu-cmd-extract.cc',
  'mu-cmd-find.cc',
  'mu-cmd-index.cc',
  'mu-cmd-script.cc',
  'mu-cmd-server.cc',
  'mu-cmd.cc',
  'mu-cmd.hh',
  'mu-config.cc',
  'mu-config.hh',
  mu_help_strings_h
],
  dependencies: [ glib_dep, lib_mu_dep, thread_dep, config_h_dep ],
  cpp_args: ['-DMU_SCRIPTS_DIR="'+ join_paths(datadir, 'mu', 'scripts') + '"'],
  install: true)

#
# tests
#

mu_binary = mu.full_path()
testmaildir=join_paths(meson.current_source_dir(),'../lib')

test('test_cmd',
     executable('test-cmd',
		'tests/test-mu-cmd.cc',
		install: false,
		cpp_args: ['-DMU_PROGRAM="' + mu_binary + '"',
			   '-DMU_TESTMAILDIR2="'+ join_paths(testmaildir, 'testdir2') + '"',
			   '-DMU_TESTMAILDIR4="'+ join_paths(testmaildir, 'testdir4') + '"'],
		dependencies: [glib_dep, lib_test_mu_common_dep, config_h_dep, lib_mu_dep]))
test('test_cmd_cfind',
     executable('test-cmd-cfind',
		'tests/test-mu-cmd-cfind.cc',
		install: false,
		cpp_args: ['-DMU_PROGRAM="' + mu_binary + '"',
			   '-DMU_TESTMAILDIR="'+ join_paths(testmaildir, 'testdir') + '"',
			  ],
		dependencies: [glib_dep, lib_test_mu_common_dep, config_h_dep]))
test('test_cmd_query',
     executable('test-cmd-query',
		'tests/test-mu-query.cc',
		install: false,
		cpp_args: ['-DMU_PROGRAM="' + mu_binary + '"',
			   '-DMU_TESTMAILDIR="'+ join_paths(testmaildir, 'testdir') + '"',
			   '-DMU_TESTMAILDIR2="'+ join_paths(testmaildir, 'testdir2') + '"'
			  ],
		dependencies: [glib_dep, lib_test_mu_common_dep, config_h_dep, lib_mu_dep]))

gmime_test = executable(
  'gmime-test', [
  'tests/gmime-test.c'
],
  dependencies: [ glib_dep, gmime_dep ],
  install: false)
