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


test('test-mu-guile',
     executable('test-mu-guile',
		'test-mu-guile.cc',
		install: false,
                cpp_args: [
                  '-DABS_SRCDIR="' + meson.current_source_dir() + '"',
                  '-DMU_GUILE_LIBRARY_PATH="' + guile_builddir + '"',
                  '-DMU_GUILE_MODULE_PATH="' + guile_builddir + '"'
                ],
		dependencies: [glib_dep, lib_mu_dep, lib_test_mu_common_dep]))
