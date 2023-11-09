## Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
# project setup
project('mu', ['c', 'cpp'],
        version: '1.10.8',
        meson_version: '>= 0.56.0',
        license: 'GPL-3.0-or-later',
        default_options : [
          'buildtype=debugoptimized',
          'warning_level=3',
          'c_std=c11',
          'cpp_std=c++17'
        ]
       )

# installation paths
prefixdir = get_option('prefix')
bindir    = prefixdir / get_option('bindir')
datadir   = prefixdir / get_option('datadir')
mandir    = prefixdir / get_option('mandir')
infodir   = prefixdir / get_option('infodir')

# allow for configuring lispdir, as with autotools.
if get_option('lispdir') == ''
  mu4e_lispdir= datadir / join_paths('emacs', 'site-lisp', 'mu4e')
else
  mu4e_lispdir= get_option('lispdir') / 'mu4e'
endif


################################################################################
# compilers / flags
#
extra_flags = [
  '-Wc11-extensions', # for clang
  '-Wno-unused-parameter',
  '-Wno-cast-function-type',
  '-Wformat-security',
  '-Wformat=2',
  '-Wstack-protector',
  '-Wno-switch-enum',
  '-Wno-keyword-macro',
  '-Wno-#warnings',
  '-Wno-volatile']

if get_option('buildtype') == 'debug'
  extra_flags += [
    '-ggdb',
    '-fvar-tracking',
    '-fvar-tracking-assignments']
endif

# compilers
cc = meson.get_compiler('c')
cxx= meson.get_compiler('cpp')

# extra arguments, if available
foreach extra_arg : extra_flags
  if cc.has_argument (extra_arg)
    add_project_arguments([extra_arg], language: 'c')
  endif
  if cxx.has_argument (extra_arg)
    add_project_arguments([extra_arg], language: 'cpp')
  endif
endforeach

# some clang don't have charconv, but we need it.
# https://github.com/djcb/mu/issues/2347
cxx.check_header('charconv', required:true)

################################################################################
# config.h setup
#
config_h_data=configuration_data()
config_h_data.set_quoted('MU_STORE_SCHEMA_VERSION', '466')
config_h_data.set_quoted('PACKAGE_VERSION', meson.project_version())
config_h_data.set_quoted('PACKAGE_STRING', meson.project_name() + ' ' +
                                           meson.project_version())
config_h_data.set_quoted('VERSION', meson.project_version())
config_h_data.set_quoted('PACKAGE_NAME', meson.project_name())

add_project_arguments(['-DHAVE_CONFIG_H'], language: 'c')
add_project_arguments(['-DHAVE_CONFIG_H'], language: 'cpp')
config_h_dep=declare_dependency(
  include_directories: include_directories(['.']))

functions=[
  'setsid'
]
foreach f : functions
  if cc.has_function(f)
    define = 'HAVE_' + f.underscorify().to_upper()
    config_h_data.set(define, 1)
  endif
endforeach

if cc.has_function('wordexp')
  config_h_data.set('HAVE_WORDEXP_H',1)
else
  message('no wordexp, expansion in command line options')
endif

testmaildir=join_paths(meson.current_source_dir(), 'lib', 'tests')
config_h_data.set_quoted('MU_TESTMAILDIR',  join_paths(testmaildir, 'testdir'))
config_h_data.set_quoted('MU_TESTMAILDIR2',  join_paths(testmaildir, 'testdir2'))
config_h_data.set_quoted('MU_TESTMAILDIR4',  join_paths(testmaildir, 'testdir4'))
config_h_data.set_quoted('MU_TESTMAILDIR_CJK',  join_paths(testmaildir, 'cjk'))

################################################################################
# hard dependencies
#
glib_dep       = dependency('glib-2.0', version: '>= 2.58')
gobject_dep    = dependency('gobject-2.0', version: '>= 2.58')
gio_dep        = dependency('gio-2.0', version: '>= 2.50')
gmime_dep      = dependency('gmime-3.0', version: '>= 3.2')
xapian_dep     = dependency('xapian-core', version:'>= 1.4')
thread_dep     = dependency('threads')

awk=find_program(['gawk', 'awk'])
gzip=find_program('gzip')

# soft dependencies
guile_dep = dependency('guile-3.0', required: get_option('guile'))

# allow for a custom guile-extension-dir
if guile_dep.found()
  custom_guile_xd=get_option('guile-extension-dir')
  if custom_guile_xd == ''
    guile_extension_dir = guile_dep.get_variable(pkgconfig: 'extensiondir')
  else
    guile_extension_dir = custom_guile_xd
  endif
  config_h_data.set_quoted('MU_GUILE_EXTENSION_DIR', guile_extension_dir)
endif

# emacs -- needed for mu4e compilation
emacs_name=get_option('emacs')
emacs=find_program([emacs_name], version: '>=26.3', required:false)
if not emacs.found()
  message('emacs not found; not pre-compiling mu4e sources')
endif

makeinfo=find_program(['makeinfo'], required:false)
if not makeinfo.found()
  message('makeinfo (texinfo) not found; not building info documentation')
else
  install_info=find_program(['install-info'], required:false)
  if not install_info.found()
    message('install-info not found')
  else
    install_info_script=join_paths(meson.current_source_dir(), 'build-aux',
                                   'meson-install-info.sh')
  endif
endif

# readline. annoyingly, macos has an incompatible libedit claiming to be
# readline. this is only a dev/debug convenience for the mu4e repl.
readline_dep=[]
if get_option('readline').enabled()
  readline_dep = dependency('readline', version:'>= 8.0')
  config_h_data.set('HAVE_LIBREADLINE', 1)
  config_h_data.set('HAVE_READLINE_READLINE_H', 1)
  config_h_data.set('HAVE_READLINE_HISTORY', 1)
  config_h_data.set('HAVE_READLINE_HISTORY_H', 1)
endif


################################################################################
# write out version.texi (for texiinfo builds in mu4e, guile)
version_texi_data=configuration_data()
version_texi_data.set('VERSION', meson.project_version())
version_texi_data.set('EDITION', meson.project_version())
version_texi_data.set('UPDATED',
                      run_command('date', '+%d %B %Y', check:true).stdout().strip())
version_texi_data.set('UPDATEDMONTH',
                      run_command('date', '+%B %Y', check:true).stdout().strip())

configure_file(input: join_paths('build-aux', 'version.texi.in'),
               output: 'version.texi',
               configuration: version_texi_data)

################################################################################
# install some data files
install_data('NEWS.org',
            install_dir : join_paths(datadir,'doc', 'mu'))

################################################################################
# subdirs
subdir('lib')
subdir('mu')

if emacs.found()
  subdir('man')
else
  message('emacs not found; not generating manpages')
endif

if emacs.found()
   subdir('mu4e')
else
  message('emacs not found; not preparing mu4e support')
endif

if not get_option('guile').disabled() and guile_dep.found()
  config_h_data.set('BUILD_GUILE', 1)
  config_h_data.set_quoted('GUILE_BINARY',
                           guile_dep.get_variable(pkgconfig: 'guile'))
  #message('guile is disabled for now')
  subdir('guile')
endif

config_h_data.set_quoted('MU_PROGRAM', mu.full_path())
################################################################################

################################################################################
# write-out config.h
configure_file(output : 'config.h', configuration : config_h_data)

if gmime_dep.version() == '3.2.13'
  warning('gmime version 3.2.13 detected, which as a decoding bug')
  warning('See: https://github.com/jstedfast/gmime/issues/133')
endif
