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


################################################################################
# tests


#
# tests
#
test('test-sexp',
     executable('test-sexp', '../mu-sexp.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-regex',
     executable('test-regex', '../mu-regex.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-command-handler',
     executable('test-command-handler', '../mu-command-handler.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-utils-file',
     executable('test-utils-file', '../mu-utils-file.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, gio_unix_dep,config_h_dep, lib_mu_utils_dep]))

test('test-logger',
     executable('test-logger', '../mu-logger.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep,config_h_dep,thread_dep ]))

test('test-option',
     executable('test-option', '../mu-option.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep ]))

test('test-lang-detector',
     executable('test-lang-detector', '../mu-lang-detector.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [ config_h_dep, glib_dep, lib_mu_utils_dep ]))

test('test-html-to-text',
     executable('test-html-to-text', '../mu-html-to-text.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-error',
     executable('test-error', '../mu-error.cc',
                install: false,
                cpp_args: ['-DBUILD_TESTS'],
                dependencies: [glib_dep, lib_mu_utils_dep]))

test('test-mu-utils',
     executable('test-mu-utils',
                'test-utils.cc',
                install: false,
                dependencies: [glib_dep, lib_mu_utils_dep]))
