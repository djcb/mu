## Copyright (C) 2021-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#
# unit tests
#

test('test-threads',
     executable('test-threads',
                '../mu-query-threads.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))
test('test-contacts-cache',
     executable('test-contacts-cache',
                '../mu-contacts-cache.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-config',
     executable('test-config',
                '../mu-config.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-query-macros',
     executable('test-query-macros',
                '../mu-query-macros.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [lib_mu_dep]))

test('test-query-processor',
     executable('test-query-processor',
                '../mu-query-processor.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [lib_mu_dep]))

test('test-query-parser',
     executable('test-query-parser',
                '../mu-query-parser.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [lib_mu_dep]))

test('test-query-xapianizer',
     executable('test-query-xapianizer',
                '../mu-query-xapianizer.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [lib_mu_dep]))


test('test-indexer',
     executable('test-indexer',
                '../mu-indexer.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, config_h_dep,
                               lib_mu_dep]))

test('test-scanner',
     executable('test-scanner',
                '../mu-scanner.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, config_h_dep,
                               lib_mu_utils_dep]))

test('test-xapian-db',
     executable('test-xapian-db',
                '../mu-xapian-db.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [lib_mu_dep, config_h_dep]))

test('test-maildir',
     executable('test-maildir',
                'test-mu-maildir.cc',
                install: false,
                dependencies: [glib_dep, lib_mu_dep]))
test('test-msg',
     executable('test-msg',
                'test-mu-msg.cc',
                install: false,
                dependencies: [glib_dep, lib_mu_dep]))
test('test-store',
     executable('test-store',
                'test-mu-store.cc',
                install: false,
                dependencies: [glib_dep, lib_mu_dep]))
test('test-query',
     executable('test-query',
                'test-query.cc',
                install: false,
                dependencies: [glib_dep, gmime_dep, lib_mu_dep]))

test('test-store-query',
     executable('test-store-query',
                'test-mu-store-query.cc',
                install: false,
                dependencies: [glib_dep, gmime_dep, lib_mu_dep]))
#
# benchmarks
#
bench_maildirs=join_paths(meson.current_build_dir(), 'maildirs')
bench_store=join_paths(meson.current_build_dir(), 'store')
bench_indexer_exe = executable(
  'bench-indexer',
  'bench-indexer.cc',
  install:false,
  cpp_args:['-DBENCH_MAILDIRS="' + bench_maildirs + '"',
            '-DBENCH_STORE="' + bench_store + '"',
           ],
  dependencies: [lib_mu_dep, glib_dep])

benchmark('bench-indexer', bench_indexer_exe, args: ['-m', 'perf'])

#
# below does _not_ pass; it is believed that it's a false alarm.
#    https://gitlab.gnome.org/GNOME/glib/-/issues/2662

# also register benchmark as a normal test so it gets included for
# valgrind/helgrind etc.
# test('test-bench-indexer', bench_indexer_exe,
#      args : ['-m', 'quick'], env: ['THREADNUM=16'])
