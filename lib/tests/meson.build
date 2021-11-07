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

#
# tests
#

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
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_store',
     executable('test-store',
		'test-mu-store.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_query',
     executable('test-query',
		'test-query.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
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
		'../mu-query-threads.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_contacts',
     executable('test-contacts',
		'../mu-contacts.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
test('test_parser',
     executable('test-parser',
		'test-parser.cc',
		install: false,
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
