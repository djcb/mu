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

#include "mu-priority.hh"

using namespace Mu;

std::string
Mu::to_string(Priority prio)
{
	return std::string{priority_name(prio)};
}

/*
 * tests... also build as runtime-tests, so we can get coverage info
 */
#ifdef BUILD_TESTS
#include <glib.h>
#define static_assert g_assert_true
#endif /*BUILD_TESTS*/

[[maybe_unused]] static void
test_priority_to_char()
{
	static_assert(to_char(Priority::Low) == 'l');
	static_assert(to_char(Priority::Normal) == 'n');
	static_assert(to_char(Priority::High) == 'h');
}

[[maybe_unused]] static void
test_priority_from_char()
{
	static_assert(priority_from_char('l') == Priority::Low);
	static_assert(priority_from_char('n') == Priority::Normal);
	static_assert(priority_from_char('h') == Priority::High);
	static_assert(priority_from_char('x') == Priority::Normal);
}

[[maybe_unused]] static void
test_priority_name()
{
	static_assert(priority_name(Priority::Low) == "low");
	static_assert(priority_name(Priority::Normal) == "normal");
	static_assert(priority_name(Priority::High) == "high");
}


#ifdef BUILD_TESTS
int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/message/priority/to-char", test_priority_to_char);
	g_test_add_func("/message/priority/from-char", test_priority_from_char);
	g_test_add_func("/message/priority/name", test_priority_name);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
