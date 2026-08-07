## Copyright (C) 2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
# bash
#
if get_option('bash-completion').allowed()
  if get_option('bash-completion-dir') == ''
    bcdir = datadir / 'bash-completion' / 'completions'
  else
    bcdir = get_option('bash-completion-dir')
  endif

  install_data('mu-completion.bash',
               rename: 'mu',
               install_dir: bcdir)
endif

#
# zsh
#
if get_option('zsh-completion').allowed()
  if get_option('zsh-completion-dir') == ''
    zcdir = datadir / 'zsh' / 'site-functions'
  else
    zcdir = get_option('zsh-completion-dir')
  endif

  install_data('mu-completion.zsh',
               rename: '_mu',
               install_dir: zcdir)
endif
