/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#if BUILD_TESTS

#include "mu-error.hh"
#include "mu-test-utils.hh"

using namespace Mu;

static void
test_fill_error()
{
	const Error err{Error::Code::Internal, "boo!"};
	GError *gerr{};

	err.fill_g_error(&gerr);

	assert_equal(gerr->message, "boo!");
	g_assert_cmpint(gerr->code, ==, static_cast<int>(err.code()));
	
	g_clear_error(&gerr);
}

static void
test_add_hint()
{
	Error err(Error::Code::Internal, "baa!");
	err.add_hint("hello");

	assert_equal(err.hint(), "hello");
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/error/fill-error", test_fill_error);
	g_test_add_func("/error/add-hint", test_add_hint);

	return g_test_run();

}

#endif /*BUILD_TESTS*/
