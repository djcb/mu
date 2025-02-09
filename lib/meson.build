## Copyright (C) 2021-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

lib_mu=static_library(
  'mu',
  [
    # db
    'mu-config.cc',
    'mu-contacts-cache.cc',
    'mu-store.cc',
    'mu-xapian-db.cc',
    # querying
    'mu-query-macros.cc',
    'mu-query-match-deciders.cc',
    'mu-query-parser.cc',
    'mu-query-processor.cc',
    'mu-query-threads.cc',
    'mu-query-xapianizer.cc',
    'mu-query.cc',
    # indexing
    'mu-indexer.cc',
    'mu-scanner.cc',
    # mu4e
    'mu-server.cc',
    # misc
    'mu-maildir.cc',
    'mu-script.cc'
  ],
  dependencies: [
    glib_dep,
    gio_dep,
    gmime_dep,
    xapian_dep,
    guile_dep,
    config_h_dep,
    lib_mu_utils_dep,
    lib_mu_message_dep],
  install: false)

lib_mu_dep = declare_dependency(
  link_with: lib_mu,
  dependencies: [ lib_mu_message_dep, thread_dep ],
  include_directories:
    include_directories(['.', '..']))

#
# dev helpers
#

process_query = executable('process-query', [ 'mu-query-processor.cc'],
  install: false,
  cpp_args: ['-DBUILD_PROCESS_QUERY'],
  dependencies: [glib_dep, lib_mu_dep])

parse_query = executable(  'parse-query', [ 'mu-query-parser.cc' ],
  install: false,
  cpp_args: ['-DBUILD_PARSE_QUERY'],
  dependencies: [glib_dep, lib_mu_dep])

parse_query_expand = executable(  'parse-query-expand', [ 'mu-query-parser.cc' ],
  install: false,
  cpp_args: ['-DBUILD_PARSE_QUERY_EXPAND'],
  dependencies: [glib_dep, lib_mu_dep])

xapian_query = executable('xapianize-query', [ 'mu-query-xapianizer.cc' ],
  install: false,
  cpp_args: ['-DBUILD_XAPIANIZE_QUERY'],
  dependencies: [glib_dep, lib_mu_dep])

list_maildirs = executable('list-maildirs', 'mu-scanner.cc',
           install: false,
           cpp_args: ['-DBUILD_LIST_MAILDIRS'],
           dependencies: [glib_dep, config_h_dep,
                               lib_mu_utils_dep])

if not get_option('tests').disabled()
  subdir('tests')
endif
