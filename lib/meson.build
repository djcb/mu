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


subdir('utils')
subdir('index')

lib_mu=static_library(
  'mu',
  [
    'mu-bookmarks.cc',
    'mu-bookmarks.hh',
    'mu-contacts.cc',
    'mu-contacts.hh',
    'mu-data.hh',
    'mu-parser.cc',
    'mu-parser.hh',
    'mu-query.cc',
    'mu-query.hh',
    'mu-query-results.hh',
    'mu-query-match-deciders.cc',
    'mu-query-match-deciders.hh',
    'mu-query-threads.cc',
    'mu-query-threads.hh',
    'mu-runtime.cc',
    'mu-runtime.hh',
    'mu-script.cc',
    'mu-script.hh',
    'mu-server.cc',
    'mu-server.hh',
    'mu-store.cc',
    'mu-store.hh',
    'mu-tokenizer.cc',
    'mu-tokenizer.hh',
    'mu-tree.hh',
    'mu-xapian.cc',
    'mu-xapian.hh',
    'mu-maildir.cc',
    'mu-maildir.hh',
    'mu-flags.cc',
    'mu-flags.hh',
    'mu-msg-crypto.cc',
    'mu-msg-doc.cc',
    'mu-msg-doc.hh',
    'mu-msg-fields.c',
    'mu-msg-fields.h',
    'mu-msg-file.cc',
    'mu-msg-file.hh',
    'mu-msg-part.cc',
    'mu-msg-part.hh',
    'mu-msg-prio.c',
    'mu-msg-prio.h',
    'mu-msg-priv.hh',
    'mu-msg-sexp.cc',
    'mu-msg.cc',
    'mu-msg.hh'
  ],
  dependencies: [
    glib_dep,
    gio_dep,
    gmime_dep,
    xapian_dep,
    config_h_dep,
    lib_mu_utils_dep,
    lib_mu_index_dep
  ],
  install: false)

lib_mu_dep = declare_dependency(
  link_with: lib_mu,
  include_directories: include_directories(['.', '..'])
)


#
# tests
#
lib_test_mu_common_inc_dep = declare_dependency(
  include_directories: include_directories(['.', '..']))
lib_test_mu_common=static_library('mu-test-common', [
				   'test-mu-common.cc',
				   'test-mu-common.hh'],
				  dependencies: [ glib_dep, thread_dep,
						  lib_test_mu_common_inc_dep])
lib_test_mu_common_dep=declare_dependency(
  link_with: lib_test_mu_common,
  include_directories: include_directories(['.']))


testmaildir=join_paths(meson.current_source_dir(),'.')

test('test_maildir',
     executable('test-maildir',
		'test-mu-maildir.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_msg_fields',
     executable('test-msg-fields',
		'test-mu-msg-fields.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_msg',
     executable('test-msg',
		'test-mu-msg.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep],
		cpp_args:['-DMU_TESTMAILDIR2="'+ join_paths(testmaildir, 'testdir2') + '"',
			  '-DMU_TESTMAILDIR4="'+ join_paths(testmaildir, 'testdir4') + '"' ]))
test('test_store',
     executable('test-store',
		'test-mu-store.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep],
		cpp_args:['-DMU_TESTMAILDIR="'+ join_paths(testmaildir, 'testdir') + '"',
			  '-DMU_TESTMAILDIR2="'+ join_paths(testmaildir, 'testdir2') + '"',
			  '-DMU_TESTMAILDIR4="'+ join_paths(testmaildir, 'testdir4') + '"']))
test('test_query',
     executable('test-query',
		'test-query.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep],
		cpp_args:['-DMU_TESTMAILDIR="'+ join_paths(testmaildir, 'testdir') + '"']))
test('test_flags',
     executable('test-flags',
		'test-mu-flags.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))

test('test_tokenizer',
     executable('test-tokenizer',
		'test-tokenizer.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_threads',
     executable('test-threads',
		'mu-query-threads.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_contacts',
     executable('test-contacts',
		'mu-contacts.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_parser',
     executable('test-parser',
		'test-parser.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
