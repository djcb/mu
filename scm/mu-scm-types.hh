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

#ifndef MU_SCM_TYPES_HH
#define MU_SCM_TYPES_HH

#include "lib/mu-store.hh"
#include "message/mu-contact.hh"

#include "mu-scm.hh"

namespace Mu::Scm {

/**
 * Initialize SCM/Store support.
 *
 * @param store a store
 */
void init_store(const Mu::Store& store);

/**
 * Initialize SCM/Message support.
 *
 * @param store a store
 */
void init_message();

/**
 * Convert a Contact to an SCM
 *
 * @param contact a contact
 *
 * @return SCM
 */
SCM to_scm(const Contact& contact);

} // Mu::Scm

#endif /*MU_SCM_TYPES_HH*/
