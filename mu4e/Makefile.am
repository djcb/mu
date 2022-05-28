## Copyright (C) 2008-2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

dist_lisp_LISP=			\
	mu4e-actions.el		\
	mu4e-bookmarks.el	\
	mu4e-compose.el		\
	mu4e-contacts.el	\
	mu4e-context.el		\
	mu4e-contrib.el		\
	mu4e-draft.el		\
	mu4e-folders.el		\
	mu4e-headers.el		\
	mu4e-icalendar.el	\
	mu4e-lists.el		\
	mu4e-helpers.el		\
	mu4e-main.el		\
	mu4e-mark.el		\
	mu4e-message.el		\
	mu4e-config.el		\
	mu4e-org.el		\
	mu4e-search.el		\
	mu4e-server.el		\
	mu4e-speedbar.el	\
	mu4e-update.el		\
	mu4e-vars.el		\
	mu4e-view.el		\
	mu4e.el			\
	obsolete/org-mu4e.el


EXTRA_DIST=			\
	mu4e-about.org

CLEANFILES=			\
	*.elc

doc_DATA =			\
	mu4e-about.org

##
## Change as needed.
##
BUILDPATH=mu4e
TEXINFO_CSS_PATH=~/Sources/ext/texinfo-css
fancyhtml:
	mkdir -p $(BUILDPATH) ; cp -R $(TEXINFO_CSS_PATH)/static $(BUILDPATH)
	makeinfo --html --css-ref=static/css/texinfo-klare.css -o $(BUILDPATH) mu4e.texi
