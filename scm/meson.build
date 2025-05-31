## Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
mu_scm_dir=join_paths(datadir, 'mu', 'scm')
mu_scm_dir_arg='-DMU_SCM_DIR="' + mu_scm_dir + '"'

lib_mu_scm=static_library(
  'mu-scm',
  [
    'mu-scm.cc',
    'mu-scm-contact.cc',
    'mu-scm-store.cc'
  ],
  dependencies: [
    guile_dep,
    config_h_dep,
    lib_mu_dep,
    lib_mu_utils_dep,
    lib_mu_message_dep],
  install: false,
  cpp_args: [mu_scm_dir_arg])

install_data(['mu-scm.scm', 'mu-scm-shell.scm'], install_dir : mu_scm_dir)

# note: top-level meson.build defines a dummy replacement for this.
mu_scm_dep = declare_dependency(
  link_with: lib_mu_scm,
  dependencies: [guile_dep, lib_mu_dep, config_h_dep, thread_dep ],
  include_directories:
    include_directories(['.', '..']))

if makeinfo.found()
  custom_target('mu_scm_info',
                input: 'mu-scm.texi',
                output: 'mu-scm.info',
                install: true,
                install_dir: infodir,
                command: [makeinfo,
                          '-o', join_paths(meson.current_build_dir(), 'mu-scm.info'),
                          join_paths(meson.current_source_dir(), 'mu-scm.texi'),
                          '-I', join_paths(meson.current_build_dir(), '..')])
  if install_info.found()
    infodir = join_paths(get_option('prefix') / get_option('infodir'))
    meson.add_install_script(install_info_script, infodir, 'mu-scm.info')
  endif
endif
