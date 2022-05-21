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


guile_load_path=':'.join([ # meson 0.56 has project_source_root
    join_paths(meson.source_root(), 'guile'),
    join_paths(meson.current_build_dir(), '..')])

test('test-mu-guile',
     executable('test-mu-guile',
		'test-mu-guile.cc',
		install: false,
                cpp_args: [
                  '-DABS_SRCDIR="' + meson.current_source_dir() + '"',
                  '-DGUILE_LOAD_PATH="' + guile_load_path + '"',
                  '-DGUILE_EXTENSIONS_PATH="' + guile_load_path + '"'
                ],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
