/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg-prio.h"


const char* 
mu_msg_prio_name (MuMsgPrio prio)
{
	switch (prio) {
	case MU_MSG_PRIO_LOW	: return "low";
	case MU_MSG_PRIO_NORMAL	: return "normal";
	case MU_MSG_PRIO_HIGH	: return "high";
	default			: g_return_val_if_reached (NULL);
	}
}

MuMsgPrio
mu_msg_prio_from_char (char k)
{
	switch (k) {
	case 'l': return MU_MSG_PRIO_LOW;
	case 'n': return MU_MSG_PRIO_NORMAL;
	case 'h': return MU_MSG_PRIO_HIGH;
	default: g_return_val_if_reached (MU_MSG_PRIO_NONE);
	}
}

char
mu_msg_prio_to_char (MuMsgPrio prio)
{
	switch (prio) {
	case MU_MSG_PRIO_LOW	: return 'l';
	case MU_MSG_PRIO_NORMAL	: return 'n';
	case MU_MSG_PRIO_HIGH	: return 'h';
	default			: g_return_val_if_reached (0);
	}
}


void
mu_msg_prio_foreach (MuMsgPrioForeachFunc func, gpointer user_data)
{
	g_return_if_fail (func);

	func (MU_MSG_PRIO_LOW, user_data);
	func (MU_MSG_PRIO_NORMAL, user_data);
	func (MU_MSG_PRIO_HIGH, user_data);
}
