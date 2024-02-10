## Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


test('test-cmd-add',
     executable('test-cmd-add',
                '../mu-cmd-add.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-cfind',
     executable('test-cmd-cfind',
                '../mu-cmd-cfind.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-extract',
     executable('test-cmd-extract',
                '../mu-cmd-extract.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-find',
     executable('test-cmd-find',
                '../mu-cmd-find.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-index',
     executable('test-cmd-index',
                '../mu-cmd-index.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-init',
     executable('test-cmd-init',
                '../mu-cmd-init.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-mkdir',
     executable('test-cmd-mkdir',
                '../mu-cmd-mkdir.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-move',
     executable('test-cmd-move',
                '../mu-cmd-move.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-remove',
     executable('test-cmd-remove',
                '../mu-cmd-remove.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-verify',
     executable('test-cmd-verify',
                '../mu-cmd-verify.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

test('test-cmd-view',
     executable('test-cmd-view',
                '../mu-cmd-view.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_dep]))

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
