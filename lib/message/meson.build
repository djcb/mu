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


lib_mu_message=static_library(
  'mu-message',
  [
    'mu-message.cc',
    'mu-message-file.cc',
    'mu-message-part.cc',
    'mu-contact.cc',
    'mu-document.cc',
    'mu-fields.cc',
    'mu-flags.cc',
    'mu-priority.cc',
    'mu-mime-object.cc',
  ],
  dependencies: [
    glib_dep,
    gmime_dep,
    xapian_dep,
    config_h_dep,
    lib_mu_utils_dep],
  install: false)

lib_mu_message_dep = declare_dependency(
  link_with: lib_mu_message,
  dependencies: [ xapian_dep, gmime_dep, lib_mu_utils_dep, config_h_dep ],
  include_directories:
    include_directories(['.', '..']))

if not get_option('tests').disabled()
  subdir('tests')
endif
