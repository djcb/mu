/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/


#include "mu-scm-contact.hh"

using namespace Mu::Scm;

SCM
Mu::Scm::to_scm(const Contact& contact)
{
	static SCM email{scm_from_utf8_symbol("email")};
	static SCM name{scm_from_utf8_symbol("name")};

	SCM alist = scm_acons(email, to_scm(contact.email), SCM_EOL);
	if (!contact.name.empty())
		alist = scm_acons(name, to_scm(contact.name), alist);

	return alist;
}
