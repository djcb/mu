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


mu4e_meta = configure_file(
  input: 'mu4e-meta.el.in',
  output: 'mu4e-meta.el',
  configuration:
    {
      'VERSION'         : meson.project_version(),
      # project_build_root() with meson >= 0.56
      'abs_top_builddir': join_paths(meson.build_root()),
      'MU_DOC_DIR'      : join_paths(datadir, 'doc', 'mu')
    })


mu4e_srcs=[
  'mu4e-actions.el',
  'mu4e-compose.el',
  'mu4e-context.el',
  'mu4e-contrib.el',
  'mu4e-draft.el',
  'mu4e.el',
  'mu4e-headers.el',
  'mu4e-icalendar.el',
  'mu4e-lists.el',
  'mu4e-main.el',
  'mu4e-mark.el',
  'mu4e-message.el',
  join_paths(meson.current_build_dir(), 'mu4e-meta.el'),
  'mu4e-org.el',
  'mu4e-proc.el',
  'mu4e-speedbar.el',
  'mu4e-utils.el',
  'mu4e-vars.el',
  'mu4e-view.el',
  'mu4e-view-gnus.el',
  'mu4e-view-old.el',
  'obsolete/org-mu4e.el',
]

foreach src : mu4e_srcs
  target_name= '@BASENAME@.elc'
  target_path = join_paths(meson.current_build_dir(), target_name)
  target_func = '(setq byte-compile-dest-file-function(lambda(_) "' + target_path + '"))'
  custom_target(src.underscorify() + '_el',
                build_by_default: true,
                input: src,
                output: target_name,
                command: [emacs,
			  '--no-init-file',
			  '--batch',
			  '--eval', target_func,
			  '--directory', meson.current_source_dir(),
			  '--funcall', 'batch-byte-compile', '@INPUT@'])
endforeach


if makeinfo.found()
  custom_target('mu4e_info',
		input: 'mu4e.texi',
		output: 'mu4e.info',
		install_dir: infodir,
		install: true,
		command: [makeinfo,
			  '-o', join_paths(meson.current_build_dir(), 'mu4e.info'),
			  join_paths(meson.current_source_dir(), 'mu4e.texi')])
endif
