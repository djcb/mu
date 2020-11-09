/*
** Copyright (C) 2019-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef MU_RESULT_HH__
#define MU_RESULT_HH__


#include "expected.hpp"
#include "utils/mu-error.hh"


namespace Mu {
/**
 * A Result is _either_ some value of type T, _or_ an error.
 *
 */
template <typename T> using Result = tl::expected<T, Error>;

} // namespace Mu


#endif /* MU_ERROR_HH__ */
