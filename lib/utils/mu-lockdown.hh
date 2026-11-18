/*
** Copyright (C) 2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_LOCKDOWN_HH
#define MU_LOCKDOWN_HH

#include "mu-result.hh"

namespace Mu {

/**
 * Lock down the running process to the bare minimum
 *
 * After lock-down, the process can only read from/write to fd (and stderr).
 * Most system calls are blocked; see mu-lockdown.cc
 *
 * If locking down fails for some reason, terminate the process.
 *
 * Currently only implemented for 64-bit Linux.
 *
 * @param fd the file-descriptor for communication
 * @param timeout_s maximum cpu-time for the process in sec, or 0 for no
 * limit. Wall-clock time is limited as well, to 3x cpu-time.
 */
void lockdown_process(int fd, size_t timeout_s = 0);

} // Mu

#endif /*MU_LOCKDOWN_HH*/
