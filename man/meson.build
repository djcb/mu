## Copyright (C) 2021-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
# generate org include files
#
man_data=configuration_data()
man_data.set('VERSION', meson.project_version())
man_data.set('YEAR', mu_year)
incs=[
  'author.inc',
  'bugs.inc',
  'common-options.inc',
  'copyright.inc.in',
  'exit-code.inc',
  'macros.inc',
  'muhome.inc',
  'prefooter.inc',
]
foreach inc: incs
  # configure the .in ones
  if inc.substring(-3) == '.in'
    configure_file(input: inc,
                   output: '@BASENAME@',
                   configuration: man_data)
  else # and copy the rest
    configure_file(input: inc, output:'@BASENAME@.inc',
                   copy:true)
  endif
endforeach

# man-pages is org-format.
man_orgs=[
  'mu.1.org',
  'mu-add.1.org',
  'mu-bookmarks.5.org',
  'mu-cfind.1.org',
  'mu-easy.7.org',
  'mu-extract.1.org',
  'mu-find.1.org',
  'mu-help.1.org',
  'mu-index.1.org',
  'mu-info.1.org',
  'mu-init.1.org',
  'mu-mkdir.1.org',
  'mu-move.1.org',
  'mu-query.7.org',
  'mu-remove.1.org',
  'mu-server.1.org',
  'mu-verify.1.org',
  'mu-view.1.org'
]

foreach src : man_orgs
  # meson makes in tricky to use the results of e.g. configure_file
  # in custom_commands..., so this is admittedly a little hacky.
  org = join_paths(meson.current_build_dir(), src)
  man = '@BASENAME@'
  section = src.substring(-5, -4)

  # we fill in some man-page details:
  #  @SECTION_ID@: the man-page section
  #  @MAN_DATE@: date of the generation (not yet supported by ox-man)
  conf_data = configuration_data()
  conf_data.set('SECTION_ID', section)
  conf_data.set('MAN_DATE', mu_month_year)
  configure_file(input: src, output:'@BASENAME@.org',
                 configuration: conf_data)

  # (replace-regexp-in-string) removes zero-width spaces — added for
  # text formatting purposes by the man-link macro — from the
  # resulting man page.
  expr_tmpl = ''.join([
    '(progn',
    '  (require \'ox-man)',
    '  (setq org-export-with-sub-superscripts \'{})',
    '  (add-to-list \'org-export-filter-plain-text-functions',
    '    (lambda (text _backend _info)',
    '      (let ((zwsp (format "%c" (char-from-name "ZERO WIDTH SPACE"))))',
    '        (replace-regexp-in-string zwsp "" text))))',
    '  (org-export-to-file \'man "@0@"))'])
  expr = expr_tmpl.format(org.substring(0,-4))
  sectiondir = join_paths(mandir, 'man' + section)

  custom_target(src + '-to-man',
                build_by_default: true,
                input: src,
                output: '@BASENAME@',
                install: true,
                install_dir: sectiondir,
                depend_files: incs,
                command: [emacs,
                          '--no-init-file',
                          '--batch',
                          org,
                          '--eval', expr])
endforeach
