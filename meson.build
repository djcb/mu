## Copyright (C) 2022-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
        version: '1.12.9',
        meson_version: '>= 0.56.0',
        license: 'GPL-3.0-or-later',
        default_options : [
          'buildtype=debugoptimized',
          'warning_level=3',
          'c_std=c11',
          'cpp_std=c++17'])

# hard-code the date here (for reproducibility); we derive the dates used in e.g.
# documentation from this.
mu_date='2025-02-26'

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

# compilers
cc = meson.get_compiler('c')
cxx= meson.get_compiler('cpp')

extra_flags = [
  '-Wno-unused-parameter',
  '-Wno-cast-function-type',
  '-Wformat-security',
  '-Wformat=2',
  '-Wstack-protector',
  '-fstack-protector-strong',
  '-Wno-switch-enum',
  # assuming these are false alarm... (in fmt, with gcc13):
  '-Wno-array-bounds',
  '-Wno-stringop-overflow',]

if (cxx.get_id() == 'clang')
  extra_flags += [
    '-Wc11-extensions',
    '-Wno-keyword-macro',
    '-Wno-deprecated-volatile',
    '-Wno-#warnings']
endif

extra_cpp_flags= [
  '-Wno-volatile'
]

if get_option('buildtype') == 'debug'
  extra_flags += [
    '-D_GLIBCXX_ASSERTIONS',
    '-ggdb',
    '-g3']
endif

# extra arguments, if available
foreach extra_arg : extra_flags
  if cc.has_argument (extra_arg)
    add_project_arguments([extra_arg], language: 'c')
  endif
endforeach

foreach extra_arg : extra_flags + extra_cpp_flags
  if cxx.has_argument (extra_arg)
    add_project_arguments([extra_arg], language: 'cpp')
  endif
endforeach

# some clang don't have charconv, but we need it.
# https://github.com/djcb/mu/issues/2347
cxx.check_header('charconv', required:true)


build_aux = join_paths(meson.current_source_dir(), 'build-aux')
################################################################################
# derived date values (based on 'mu-date'); used in docs
# we can't use the 'date' because MacOS 'date' is incompatible with GNU's.
pdate=find_program(join_paths(build_aux, 'date.py'))
env = environment()
env.set('LANG', 'C')
mu_day_month_year = run_command(pdate, mu_date, '%d %B %Y',
                                check:true, capture:true,
                                env: env).stdout().strip()
mu_month_year = run_command(pdate, mu_date, '%B %Y',
                            check:true, capture:true,
                            env: env).stdout().strip()
mu_year = run_command(pdate, mu_date, '%Y',
                      check:true, capture:true, env: env).stdout().strip()

################################################################################
# config.h setup
#
config_h_data=configuration_data()
config_h_data.set('MU_STORE_SCHEMA_VERSION', 500)
config_h_data.set_quoted('PACKAGE_VERSION', meson.project_version())
config_h_data.set_quoted('PACKAGE_STRING', meson.project_name() + ' ' +
                                           meson.project_version())
config_h_data.set_quoted('VERSION', meson.project_version())
config_h_data.set_quoted('PACKAGE_NAME', meson.project_name())

add_project_arguments(['-DHAVE_CONFIG_H'], language: 'c')
add_project_arguments(['-DHAVE_CONFIG_H'], language: 'cpp')
config_h_dep=declare_dependency(
  include_directories: include_directories(['.']))

#
# d_type, d_ino are not available universally, so let's check
# (we use them for optimizations in mu-scanner
#
if cxx.has_member('struct dirent', 'd_ino', prefix : '#include<dirent.h>')
  config_h_data.set('HAVE_DIRENT_D_INO', 1)
endif

if cxx.has_member('struct dirent', 'd_type', prefix : '#include<dirent.h>')
  config_h_data.set('HAVE_DIRENT_D_TYPE', 1)
endif


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
  message('no wordexp, no command-line option expansion')
endif

if not get_option('tests').disabled()
  # only needed for tests
  cp=find_program('cp')
  ln=find_program('ln')
  rm=find_program('rm')

  config_h_data.set_quoted('CP_PROGRAM',   cp.full_path())
  config_h_data.set_quoted('RM_PROGRAM',   rm.full_path())
  config_h_data.set_quoted('LN_PROGRAM',   ln.full_path())

  testmaildir=join_paths(meson.current_source_dir(), 'testdata')
  config_h_data.set_quoted('MU_TESTMAILDIR',  join_paths(testmaildir, 'testdir'))
  config_h_data.set_quoted('MU_TESTMAILDIR2',  join_paths(testmaildir, 'testdir2'))
  config_h_data.set_quoted('MU_TESTMAILDIR4',  join_paths(testmaildir, 'testdir4'))
  config_h_data.set_quoted('MU_TESTMAILDIR_CJK',  join_paths(testmaildir, 'cjk'))
endif


################################################################################
# hard dependencies
#
glib_dep       = dependency('glib-2.0', version: '>= 2.60')
gobject_dep    = dependency('gobject-2.0', version: '>= 2.60')
gio_dep        = dependency('gio-2.0', version: '>= 2.60')
gio_unix_dep   = dependency('gio-unix-2.0', version: '>= 2.60')
gmime_dep      = dependency('gmime-3.0', version: '>= 3.2')
thread_dep     = dependency('threads')

# we need Xapian 1.4
xapian_dep = dependency('xapian-core', version:'>= 1.4', required:true)
xapver = xapian_dep.version()
if xapver.version_compare('>= 1.4.6')
  message('xapian ' + xapver + ' supports c++ move-semantics')
  config_h_data.set('HAVE_XAPIAN_MOVE_SEMANTICS', 1)
endif
if xapver.version_compare('>= 1.4.23')
  message('xapian ' + xapver + ' supports ngrams')
  config_h_data.set('HAVE_XAPIAN_FLAG_NGRAMS', 1)
endif

host_system = host_machine.system()
#
# soft dependencies
#


# logging

# if we're on a linux machine, perhaps there's systemd/journald.
# otherwise, we don't bother.
if host_machine.system() == 'linux'
  config_h_data.set('MAYBE_USE_JOURNAL', 1)
endif
if cc.has_function('g_log_writer_syslog',dependencies: glib_dep)
  config_h_data.set('MAYBE_USE_SYSLOG', 1)
endif

# optionally, use Compact Language Detector2 if we can find it.
cld2_dep = meson.get_compiler('cpp').find_library('cld2', required: get_option('cld2'))
if not get_option('cld2').disabled() and cld2_dep.found()
  config_h_data.set('HAVE_CLD2', 1)
else
  message('CLD2 not found or disabled; no support for language detection')
endif

# guile
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
  message('Using guile-extension-dir: ' + guile_extension_dir)
endif

makeinfo=find_program(['makeinfo'], required:false)
if not makeinfo.found()
  message('makeinfo (texinfo) not found; not building info documentation')
else
  install_info=find_program(['install-info'], required:false)
  if not install_info.found()
    message('install-info not found')
  else
    install_info_script=join_paths(build_aux, 'meson-install-info.sh')
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
# write out version.texi (for texinfo builds in mu4e, guile)
version_texi_data=configuration_data()
version_texi_data.set('VERSION', meson.project_version())
version_texi_data.set('EDITION', meson.project_version())

# derived date values
version_texi_data.set('UPDATED', mu_day_month_year)
version_texi_data.set('UPDATEDMONTH', mu_month_year)
version_texi_data.set('UPDATEDYEAR', mu_year)

configure_file(input: join_paths(build_aux, 'version.texi.in'),
               output: 'version.texi',
               configuration: version_texi_data)

################################################################################
# install some data files
install_data('NEWS.org', 'IDEAS.org',
             install_dir : join_paths(datadir, 'doc', 'mu'))

################################################################################
# subdirs
subdir('lib')
subdir('mu')

# emacs -- needed for mu4e compilation
emacs_name=get_option('emacs')
emacs_min_version='26.3'
emacs=find_program([emacs_name], version: '>=' + emacs_min_version, required:false)
if emacs.found()
  emacs28 = find_program([emacs_name], version: '>=28', required:false)
  subdir('man')
  subdir('mu4e')
else
  message('emacs not found; not pre-compiling mu4e / generating manpages')
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
  warning('gmime version 3.2.13 detected, which has a decoding bug')
  warning('See: https://github.com/jstedfast/gmime/issues/133')
endif

# Local Variables:
# indent-tabs-mode: nil
# End:
