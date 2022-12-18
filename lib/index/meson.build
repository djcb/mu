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

index_srcs=[
	'mu-indexer.hh',
	'mu-indexer.cc',
	'mu-scanner.hh',
	'mu-scanner.cc'
]

xapian_incs = xapian_dep.get_variable(pkgconfig: 'includedir')
lib_mu_index_inc_dep = declare_dependency(
  include_directories: include_directories(['.', '..', xapian_incs]))
lib_mu_index=static_library('mu-index', [index_srcs],
			    dependencies: [
			      config_h_dep,
			      glib_dep,
			      lib_mu_index_inc_dep
			    ],
			    install: false)

lib_mu_index_dep = declare_dependency(
  link_with: lib_mu_index
)
