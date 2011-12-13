## Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

BUILT_SOURCES=mu4e-version.el

mu4e-version.el:
	@echo -e ";; auto-generated\n\
(defconst mu4e-mu-version \"$(VERSION)\" \"Required mu binary version.\")\n\
(provide 'mu4e-version)\n" >$@

EXTRA_DIST=			\
	mu4e.el			\
	mu4e-hdrs.el		\
	mu4e-view.el		\
	mu4e-proc.el		\
	mu4e-send.el		\
	mu4e-version.el
