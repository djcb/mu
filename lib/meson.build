## Copyright (C) 2021-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
subdir('message')
subdir('index')

lib_mu=static_library(
  'mu',
  [
    'mu-bookmarks.cc',
    'mu-contacts-cache.cc',
    'mu-maildir.cc',
    'mu-parser.cc',
    'mu-query-match-deciders.cc',
    'mu-query-threads.cc',
    'mu-query.cc',
    'mu-script.cc',
    'mu-server.cc',
    'mu-store.cc',
    'mu-tokenizer.cc',
    'mu-xapian.cc'
  ],
  dependencies: [
    glib_dep,
    gio_dep,
    gmime_dep,
    xapian_dep,
    guile_dep,
    config_h_dep,
    lib_mu_utils_dep,
    lib_mu_message_dep,
    lib_mu_index_dep
  ],
  install: false)


lib_mu_dep = declare_dependency(
  link_with: lib_mu,
  dependencies: [ lib_mu_message_dep, thread_dep ],
  include_directories:
    include_directories(['.', '..']))

# dev helpers
tokenize = executable(
  'tokenize',
  [ 'mu-tokenizer.cc', 'tokenize.cc' ],
  dependencies: [ lib_mu_utils_dep, glib_dep ],
  install: false)

# actual tests

test('test-threads',
     executable('test-threads',
		'mu-query-threads.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep]))
test('test-contacts-cache',
     executable('test-contacts-cache',
		'mu-contacts-cache.cc',
		install: false,
		cpp_args: ['-DBUILD_TESTS'],
		dependencies: [glib_dep, lib_mu_dep]))

subdir('tests')
