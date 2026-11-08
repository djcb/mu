## Copyright (C) 2022-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

if lockdown_helper_dep.found()
  srcs += [ 'mu-lockdown.cc' ]
endif

# only with pdf-to-text support; otherwise, there is a dummy in mu-utils.cc
# Note, _without_ the poppler-using parts (BUILD_PDF_TO_TEXT_INTERNAL): only the
# mu-pdf2text helper should link with poppler.
if poppler_dep.found()
  srcs += [ 'mu-pdf-to-text.cc' ]
endif

if get_option('tests').allowed()
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
  cld2_dep,
  lockdown_helper_dep,
  fmt_dep,
  tl_expected_dep,
  tl_optional_dep
], include_directories:
     include_directories(['.', '..']),
install: false)

lib_mu_utils_dep = declare_dependency(
  link_with: lib_mu_utils,
  # tl-expected/tl-optional propagate: mu-result.hh/mu-option.hh need them
  # lockdown_helper_dep propagates for linking the static lib
  dependencies: [fmt_dep, tl_expected_dep, tl_optional_dep, lockdown_helper_dep],
  include_directories:
    include_directories(['.', '..']))

#
# tools
#
make_embeddable = find_program(build_aux / 'make-embeddable')

html2text = executable('mu-html2text',
                       'mu-html-to-text.cc',
  dependencies: [ lib_mu_utils_dep, glib_dep ],
  cpp_args: ['-DBUILD_HTML_TO_TEXT'],
  install: false)

# we install the mu-pdf2text binary, we use it a runtime; it is the only thing
# that links with poppler
if poppler_dep.found()
  # the tiny pdf we use for warming up poppler, as something we can #include
  poppler_warmup_inc = custom_target('poppler-warmup-inc',
    input: 'poppler-warmup.pdf',
    output: 'poppler-warmup.pdf.inc',
    command: [make_embeddable, '@INPUT@', 'warm_up_pdf'],
    capture: true)
  pdf2text = executable('mu-pdf2text',
                        ['mu-pdf2text.cc',
                         'mu-pdf-to-text.cc',
                         poppler_warmup_inc],
    cpp_args: ['-DBUILD_PDF_TO_TEXT_INTERNAL'],
    dependencies: [ lib_mu_utils_dep, glib_dep, poppler_dep, config_h_dep, cli11_dep ],
    install: true,
    install_dir: get_option('libexecdir'))
  # for tests, use the binary from the build-tree
  pdf2text_env = ['MU_PDF_TO_TEXT_PROGRAM=' + pdf2text.full_path()]
else
  pdf2text = []
  pdf2text_env = []
endif

if get_option('tests').allowed()
  subdir('tests')
endif
