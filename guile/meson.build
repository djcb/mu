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

#
# create a shell script for compiling from the source dirs
compile_scm_conf = configuration_data()
compile_scm_conf.set('abs_builddir', meson.current_build_dir())
compile_scm_conf.set('guild', 'guild')
compile_scm=configure_file(
  input: 'compile-scm.in',
  output: 'compile-scm',
  configuration: compile_scm_conf,
  install: false
)
run_command('chmod', '+x', compile_scm)
scm_compiler=join_paths(meson.current_build_dir(), 'compile-scm')

snarf = find_program('guile-snarf')
snarf_args=['-o', '@OUTPUT@', '@INPUT@', '-I' + meson.current_source_dir() + '/..',
	    '-I' + meson.current_source_dir() + '/../lib',
	    '-I' + meson.current_build_dir() + '/..']

pkg_config=find_program('pkg-config')
snarf_args+=run_command(pkg_config, '--cflags', 'glib-2.0', 'guile-3.0').stdout().strip()
snarf_gen=generator(snarf,
		    output: '@BASENAME@.x',
		    arguments: snarf_args)

snarf_srcs=['mu-guile.cc', 'mu-guile-message.cc']
snarf_x=snarf_gen.process(snarf_srcs)

lib_guile_mu = shared_module(
  'guile-mu',
  [ 'mu-guile.cc',
    'mu-guile.hh',
    'mu-guile-message.cc',
    'mu-guile-message.hh',
    snarf_x ],
  dependencies: [guile_dep, glib_dep, lib_mu_dep, config_h_dep, thread_dep ],
  install: true)

if makeinfo.found()
  custom_target('mu_guile_info',
		input: 'mu-guile.texi',
		output: 'mu-guile.info',
		install: true,
		install_dir: infodir,
		command: [makeinfo,
			  '-o', join_paths(meson.current_build_dir(), 'mu-guile.info'),
			  join_paths(meson.current_source_dir(), 'mu-guile.texi'),
			  '-I', join_paths(meson.current_build_dir(), '..')])
endif

guile_scm_dir=join_paths(datadir, 'guile', 'site', '3.0', 'mu')
install_data(['mu.scm','mu/script.scm', 'mu/message.scm', 'mu/stats.scm', 'mu/plot.scm'],
	     install_dir: guile_scm_dir)

guile_builddir=meson.current_build_dir()

subdir('tests')
