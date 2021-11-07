/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <vector>
#include <glib.h>

#include <iostream>
#include <sstream>
#include <unistd.h>

#include "mu-indexer.hh"
#include "utils/mu-utils.hh"
#include "test-mu-common.h"

using namespace Mu;

static void
test_index_maildir()
{
	allow_warnings();

	Store   store{test_mu_common_get_random_tmpdir(), std::string{MU_TESTMAILDIR}};
	Indexer idx{Indexer::Config{}, store};

	g_assert_true(idx.start());
	while (idx.is_running()) {
		sleep(1);
	}

	g_print("again!\n");

	g_assert_true(idx.start());
	while (idx.is_running()) {
		sleep(1);
	}
}

int
main(int argc, char* argv[])
try {
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/indexer/index-maildir", test_index_maildir);

	return g_test_run();

} catch (const std::runtime_error& re) {
	std::cerr << re.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "caught exception\n";
	return 1;
}
