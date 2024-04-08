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

thirdparty=join_paths('..', '..', 'thirdparty')

srcs = [
  'mu-command-handler.cc',
  'mu-html-to-text.cc',
  'mu-lang-detector.cc',
  'mu-logger.cc',
  'mu-option.cc',
  'mu-readline.cc',
  'mu-sexp.cc',
  'mu-utils-file.cc',
  'mu-utils.cc',
]

if not get_option('tests').disabled()
  test_srcs =  [ 'mu-test-utils.cc' ]
else
  test_srcs =  []
endif

lib_mu_utils=static_library('mu-utils',
 [ srcs, test_srcs ], dependencies: [
  glib_dep,
  gio_dep,
  gio_unix_dep,
  config_h_dep,
  readline_dep,
  cld2_dep
], include_directories:
     include_directories(['.', '..', thirdparty]),
install: false)

lib_mu_utils_dep = declare_dependency(
  link_with: lib_mu_utils,
  compile_args: '-DFMT_HEADER_ONLY',
  include_directories:
    include_directories(['.', '..', thirdparty]))

#
# tools
#
html2text = executable('mu-html2text',
                       'mu-html-to-text.cc',
  dependencies: [ lib_mu_utils_dep, glib_dep ],
  cpp_args: ['-DBUILD_HTML_TO_TEXT'],
  install: false)

if not get_option('tests').disabled()
  subdir('tests')
endif
