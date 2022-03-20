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

awk_script=join_paths(meson.current_source_dir(), 'mu-help-strings.awk')
mu_help_strings_h=custom_target('mu_help',
				input: 'mu-help-strings.txt',
				output: 'mu-help-strings.inc',
				command: [awk, '-f', awk_script, '@INPUT@'],
				capture: true)
mu = executable(
  'mu', [
  'mu.cc',
  'mu-cmd-cfind.cc',
  'mu-cmd-extract.cc',
  'mu-cmd-find.cc',
  'mu-cmd-index.cc',
  'mu-cmd-script.cc',
  'mu-cmd-server.cc',
  'mu-cmd.cc',
  'mu-cmd.hh',
  'mu-config.cc',
  'mu-config.hh',
  mu_help_strings_h
],
  dependencies: [ glib_dep, gmime_dep, lib_mu_dep, thread_dep, config_h_dep ],
  cpp_args: ['-DMU_SCRIPTS_DIR="'+ join_paths(datadir, 'mu', 'scripts') + '"'],
  install: true)

subdir('tests')
