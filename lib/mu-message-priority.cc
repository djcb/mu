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

#ifndef MU_MESSAGE_PRIORITY_CC__
#define MU_MESSAGE_PRIORITY_CC__

#include "mu-message-priority.hh"

using namespace Mu;

std::string
Mu::to_string(MessagePriority prio)
{
	return std::string{message_priority_name(prio)};
}


static_assert(to_char(MessagePriority::Low) == 'l');
static_assert(to_char(MessagePriority::Normal) == 'n');
static_assert(to_char(MessagePriority::High) == 'h');

static_assert(message_priority_from_char('l') == MessagePriority::Low);
static_assert(message_priority_from_char('n') == MessagePriority::Normal);
static_assert(message_priority_from_char('h') == MessagePriority::High);
static_assert(message_priority_from_char('x') == MessagePriority::Normal);

static_assert(message_priority_name(MessagePriority::Low) == "low");
static_assert(message_priority_name(MessagePriority::Normal) == "normal");
static_assert(message_priority_name(MessagePriority::High) == "high");


#endif /* MU_MESSAGE_PRIORITY_CC__ */
