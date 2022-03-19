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


subdir('utils')
subdir('index')

lib_mu=static_library(
  'mu',
  [
    'mu-bookmarks.cc',
    'mu-bookmarks.hh',
    'mu-contacts-cache.cc',
    'mu-contacts-cache.hh',
    'mu-data.hh',
    'mu-parser.cc',
    'mu-parser.hh',
    'mu-query.cc',
    'mu-query.hh',
    'mu-query-results.hh',
    'mu-query-match-deciders.cc',
    'mu-query-match-deciders.hh',
    'mu-query-threads.cc',
    'mu-query-threads.hh',
    'mu-runtime.cc',
    'mu-runtime.hh',
    'mu-script.cc',
    'mu-script.hh',
    'mu-server.cc',
    'mu-server.hh',
    'mu-store.cc',
    'mu-store.hh',
    'mu-tokenizer.cc',
    'mu-tokenizer.hh',
    'mu-tree.hh',
    'mu-xapian.cc',
    'mu-xapian.hh',
    'mu-maildir.cc',
    'mu-maildir.hh',
    'mu-msg-crypto.cc',
    'mu-msg-doc.cc',
    'mu-msg-doc.hh',
    'mu-msg-file.cc',
    'mu-msg-file.hh',
    'mu-msg-part.cc',
    'mu-msg-part.hh',
    'mu-msg-priv.hh',
    'mu-msg-sexp.cc',
    'mu-msg.cc',
    'mu-msg.hh',
    'mu-message-contact.hh',
    'mu-message-contact.cc',
    'mu-message-document.cc',
    'mu-message-document.hh',
    'mu-message-fields.hh',
    'mu-message-fields.cc',
    'mu-message-flags.hh',
    'mu-message-flags.cc',
    'mu-message-priority.hh',
    'mu-message-priority.cc'
  ],
  dependencies: [
    glib_dep,
    gio_dep,
    gmime_dep,
    xapian_dep,
    guile_dep,
    config_h_dep,
    lib_mu_utils_dep,
    lib_mu_index_dep
  ],
  install: false)

# some of the libme headers include xapian
xapian_incs = xapian_dep.get_pkgconfig_variable('includedir')
lib_mu_dep = declare_dependency(
  link_with: lib_mu,
  include_directories:
    include_directories(['.', '..', xapian_incs]))

# test helpers
lib_test_mu_common=static_library('mu-test-common', [
				   'tests/test-mu-common.cc',
				   'tests/test-mu-common.hh'],
				  dependencies: [ glib_dep, thread_dep,
                                                  config_h_dep])
lib_test_mu_common_dep=declare_dependency(
  link_with: lib_test_mu_common,
  include_directories: include_directories(['tests']))

# actual tests
subdir('tests')
