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


# generate some build data for use in mu4e
version_extra=''

mu4e_meta = configure_file(
  input: 'mu4e-config.el.in',
  output: 'mu4e-config.el',
  install: true,
  install_dir: mu4e_lispdir,
  configuration: {
      'VERSION'           : meson.project_version(),
      'MU_VERSION_EXTRA'  : version_extra,
      'MU_DOC_DIR'        : join_paths(datadir, 'doc', 'mu'),
    })

mu4e_pkg_desc = configure_file(
  input: 'mu4e-pkg.el.in',
  output: 'mu4e-pkg.el',
  install: true,
  install_dir: mu4e_lispdir,
  configuration: {
      'VERSION'           : meson.project_version(),
      'EMACS_MIN_VERSION' : emacs_min_version,
    })

mu4e_srcs=[
  'mu4e-actions.el',
  'mu4e-bookmarks.el',
  'mu4e-compose.el',
  'mu4e-contacts.el',
  'mu4e-context.el',
  'mu4e-contrib.el',
  'mu4e-draft.el',
  'mu4e-folders.el',
  'mu4e.el',
  'mu4e-headers.el',
  'mu4e-helpers.el',
  'mu4e-icalendar.el',
  'mu4e-lists.el',
  'mu4e-main.el',
  'mu4e-mark.el',
  'mu4e-message.el',
  'mu4e-mime-parts.el',
  'mu4e-modeline.el',
  'mu4e-notification.el',
  'mu4e-obsolete.el',
  'mu4e-org.el',
  'mu4e-query-items.el',
  'mu4e-search.el',
  'mu4e-server.el',
  'mu4e-speedbar.el',
  'mu4e-thread.el',
  'mu4e-update.el',
  'mu4e-vars.el',
  'mu4e-view.el',
  'mu4e-window.el'
]

# emacs 28 is guaranteed to have transient
# not very elegant, but
# https://stackoverflow.com/questions/49221792/byte-compile-file-only-when-library-is-found
if emacs28.found()
  mu4e_srcs += 'mu4e-transient.el'
endif

# note, we cannot compile mu4e-config.el without incurring
#  WARNING: Source item
#   '[...]/build/mu4e/mu4e-meta.el' cannot be converted to File object, because
#   it is a generated file. This will become a hard error in the future.
#
#... so let's not do that!

foreach src : mu4e_srcs
  target_name= '@BASENAME@.elc'
  target_path = join_paths(meson.current_build_dir(), target_name)
  target_func = '(setq byte-compile-dest-file-function(lambda(_) "' + target_path + '"))'

  # hack-around for native compile issue: copy sources to builddir.
  # see: https://debbugs.gnu.org/db/47/47987.html
  configure_file(input: src, output:'@BASENAME@.el', copy:true,
                 install_mode: 'r--r--r--')
  # seems meson ignores 'install_mode' for copy (1.5.1, 1.6.1)

  custom_target(src.underscorify() + '_el',
                build_by_default: true,
                input: src,
                output: target_name,
                install_dir: mu4e_lispdir,
                install: true,
                # rebuild all if any changed.
                depend_files: mu4e_srcs,
                command: [emacs,
                          '--no-init-file',
                          '--batch',
                          '--directory', meson.current_source_dir(),
                          '--directory', meson.current_build_dir(),
                          # we don't need warnings for items that have become
                          # obsolete _after_ our last supported emacs release.
                          '--eval', '(setq byte-compile-warnings \'(not obsolete))',
                          '--eval', target_func,
                          '--funcall', 'batch-byte-compile', '@INPUT@'])

endforeach

# this depends on the above hack: all mu4e elisp files needs to be in builddir
mu4e_autoloads = configure_file(
  output: 'mu4e-autoloads.el',
  install: true,
  install_dir: mu4e_lispdir,
  command: [emacs,
            '--no-init-file',
            '--batch',
            '--load', 'package',
            '--eval', '(package-generate-autoloads "mu4e" "' +
                      meson.current_build_dir() + '" )'])

# also install the sources and the config
install_data(mu4e_srcs, install_dir: mu4e_lispdir)

# install mu4e-about.org
install_data('mu4e-about.org', install_dir : join_paths(datadir, 'doc', 'mu'))

if makeinfo.found()
  custom_target('mu4e_info',
                input: 'mu4e.texi',
                output: 'mu4e.info',
                install_dir: infodir,
                install: true,
                command: [makeinfo,
                          '-o', join_paths(meson.current_build_dir(), 'mu4e.info'),
                          join_paths(meson.current_source_dir(), 'mu4e.texi'),
                          '-I', join_paths(meson.current_build_dir(), '..')])
  if install_info.found()
    infodir = join_paths(get_option('prefix') / get_option('infodir'))
    meson.add_install_script(install_info_script, infodir, 'mu4e.info')
  endif
endif
