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
if not get_option('tests').disabled()
  subdir('tests')
endif
