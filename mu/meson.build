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

mu = executable(
  'mu', [
  'mu.cc',
  'mu-options.cc',
  'mu-cmd-add.cc',
  'mu-cmd-cfind.cc',
  'mu-cmd-extract.cc',
  'mu-cmd-find.cc',
  'mu-cmd-info.cc',
  'mu-cmd-init.cc',
  'mu-cmd-index.cc',
  'mu-cmd-mkdir.cc',
  'mu-cmd-move.cc',
  'mu-cmd-remove.cc',
  'mu-cmd-script.cc',
  'mu-cmd-server.cc',
  'mu-cmd-verify.cc',
  'mu-cmd-view.cc',
  'mu-cmd.cc'
],
  dependencies: [ glib_dep, gmime_dep, lib_mu_dep, thread_dep, config_h_dep ],
  cpp_args: ['-DMU_SCRIPTS_DIR="'+ join_paths(datadir, 'mu', 'scripts') + '"'],
  install: true)

#
# tests
#

test('test-cmd-add',
     executable('test-cmd-add',
                'mu-cmd-add.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-cfind',
     executable('test-cmd-cfind',
                'mu-cmd-cfind.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-extract',
     executable('test-cmd-extract',
                'mu-cmd-extract.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-find',
     executable('test-cmd-find',
                'mu-cmd-find.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-index',
     executable('test-cmd-index',
                'mu-cmd-index.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-init',
     executable('test-cmd-init',
                'mu-cmd-init.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-mkdir',
     executable('test-cmd-mkdir',
                'mu-cmd-mkdir.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-move',
     executable('test-cmd-move',
                'mu-cmd-move.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-remove',
     executable('test-cmd-remove',
                'mu-cmd-remove.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-verify',
     executable('test-cmd-verify',
                'mu-cmd-verify.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-view',
     executable('test-cmd-view',
                'mu-cmd-view.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

subdir('tests')
