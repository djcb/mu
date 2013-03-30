## Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

include $(top_srcdir)/gtest.mk

SUBDIRS=

info_TEXINFOS=mu4e.texi
mu4e_TEXINFOS=fdl.texi

lispdir=${prefix}/share/emacs/site-lisp/mu4e/

dist_lisp_LISP=			\
	mu4e-about.el		\
	mu4e-actions.el		\
	mu4e-compose.el		\
	mu4e-draft.el		\
	mu4e-headers.el		\
	mu4e-lists.el		\
	mu4e-main.el		\
	mu4e-mark.el		\
	mu4e-message.el         \
	mu4e-meta.el		\
	mu4e-proc.el		\
	mu4e-speedbar.el	\
	mu4e-utils.el		\
	mu4e-vars.el		\
	mu4e-view.el		\
	mu4e.el			\
	org-mu4e.el

mu4e-about.el: mu4e-about.org
	@echo ";; auto-generated"           >   mu4e-about.el
	@echo "(defconst mu4e-about \""     >>  mu4e-about.el
	@sed 's/"/\\"/g' < mu4e-about.org   >>  mu4e-about.el
	@echo "\" \"About mu4e.\")"         >>  mu4e-about.el
	@echo "(provide 'mu4e-about)"       >>  mu4e-about.el

EXTRA_DIST=$(elisp_DATA) mu4e-about.org

CLEANFILES=*.elc ${BUILT_SOURCES}
