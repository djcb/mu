/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/

#include "mu-utils.hh"
#include "mu-option.hh"

using namespace Mu;

static Option<int>
get_opt_int(bool b)
{
	if (b)
		return Some(123);
	else
		return Nothing;
}

static void
test_option()
{
	{
		const auto oi{get_opt_int(true)};
		g_assert_true(!!oi);
		g_assert_cmpint(oi.value(), ==, 123);
	}

	{
		const auto oi{get_opt_int(false)};
		g_assert_false(!!oi);
		g_assert_false(oi.has_value());
		g_assert_cmpint(oi.value_or(456), ==, 456);
	}
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/option/option", test_option);

	return g_test_run();
}
