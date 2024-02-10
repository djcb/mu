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
run_command('chmod', '+x', compile_scm, check: true)
scm_compiler=join_paths(meson.current_build_dir(), 'compile-scm')

#
# NOTE: snarfing works but you get:
# ,----
# | cc1plus: warning: command-line option ‘-std=gnu11’ is valid for C/ObjC
# | but not for C++
# `----
# this is because the snarf-script hardcodes the '-std=gnu11' but we're
# building for c++; even worse, e.g. on some MacOS, the warning is a
# hard error.
#
# We can override flag through a env variable CPP; but then we _also_ need to
# override the compiler, so e.g. CPP="g++ -std=c++17'; but it's a bit
# hairy/ugly/fragile to derive the raw compiler name in meson; also the
# generator expression doesn't take an 'env:' parameter, so we'd need
# to rewrite using custom_target...
#
# for now, we avoid all that by simply including the generated files.
do_snarf=false

if do_snarf
  snarf = find_program('guile-snarf3.0','guile-snarf')
  # there must be a better way of feeding the include paths to snarf...
  snarf_args=['-o', '@OUTPUT@', '@INPUT@', '-I' + meson.current_source_dir() + '/..',
              '-I' + meson.current_source_dir() + '/../lib',
              '-I' + meson.current_build_dir() + '/..']
  snarf_args += '-I' + join_paths(glib_dep.get_pkgconfig_variable('includedir'),
                                  'glib-2.0')
  snarf_args += '-I' + join_paths(glib_dep.get_pkgconfig_variable('libdir'),
                                  'glib-2.0', 'include')
  snarf_args += '-I' + join_paths(guile_dep.get_pkgconfig_variable('includedir'),
                                  'guile', '3.0')
  snarf_gen=generator(snarf,
                      output: '@BASENAME@.x',
                      arguments: snarf_args)
  snarf_srcs=['mu-guile.cc', 'mu-guile-message.cc']
  snarf_x=snarf_gen.process(snarf_srcs)
else
  snarf_x = [ 'mu-guile-message.x', 'mu-guile.x' ]
endif

lib_guile_mu = shared_module(
  'guile-mu',
  [ 'mu-guile.cc',
    'mu-guile-message.cc' ],
  dependencies: [guile_dep, glib_dep, lib_mu_dep, config_h_dep, thread_dep ],
  install: true,
  install_dir: guile_extension_dir
)

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
  if install_info.found()
    infodir = join_paths(get_option('prefix') / get_option('infodir'))
    meson.add_install_script(install_info_script, infodir, 'mu-guile.info')
  endif
endif

guile_scm_dir=join_paths(datadir, 'guile', 'site', '3.0')
install_data(['mu.scm'], install_dir: guile_scm_dir)
guile_scm_mu_dir=join_paths(guile_scm_dir, 'mu')
foreach mod : ['script.scm', 'message.scm', 'stats.scm', 'plot.scm']
    install_data(join_paths('mu', mod), install_dir: guile_scm_mu_dir)
endforeach

mu_guile_scripts=[
  join_paths('scripts', 'find-dups.scm'),
  join_paths('scripts', 'msgs-count.scm'),
  join_paths('scripts', 'histogram.scm')]
mu_guile_script_dir=join_paths(datadir, 'mu', 'scripts')
install_data(mu_guile_scripts, install_dir: mu_guile_script_dir)

guile_builddir=meson.current_build_dir()

if not get_option('tests').disabled()
  subdir('tests')
endif
