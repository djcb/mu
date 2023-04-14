## Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
  'mu-command-handler.cc',
  'mu-logger.cc',
  'mu-option.cc',
  'mu-readline.cc',
  'mu-sexp.cc',
  'mu-test-utils.cc',
  'mu-utils-file.cc',
  'mu-utils.cc'
], dependencies: [
  glib_dep,
  gio_dep,
  config_h_dep,
  readline_dep
], include_directories:
     include_directories(['.','..']),
install: false)

lib_mu_utils_dep = declare_dependency(
  link_with: lib_mu_utils,
  include_directories: include_directories(['.', '..'])
)

#
# tests
#
test('test-sexp',
     executable('test-sexp', 'mu-sexp.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-regex',
     executable('test-regex', 'mu-regex.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-command-handler',
     executable('test-command-handler', 'mu-command-handler.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-utils-file',
     executable('test-utils-file', 'mu-utils-file.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, config_h_dep, lib_mu_utils_dep]))

test('test-logger',
     executable('test-logger', 'mu-logger.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep, thread_dep ]))

subdir('tests')
