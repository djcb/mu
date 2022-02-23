/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_MESSAGE_HH__
#define MU_MESSAGE_HH__

#include "mu-message-contact.hh"
#include "mu-message-priority.hh"
#include "mu-message-flags.hh"
#include "mu-message-fields.hh"

namespace Mu {

namespace Message {

using   Contact  = MessageContact;
using   Contacts = MessageContacts;
using	Priority = MessagePriority;
using	Flags	 = MessageFlags;
using	Field	 = MessageField;

} // Message
} // Mu
#endif /* MU_MESSAGE_HH__ */
