/*
** Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <vector>
#include <glib.h>

#include <iostream>
#include <sstream>

#include "mu-scanner.hh"
#include "mu-utils.hh"

using namespace Mu;

static void
test_scan_maildir()
{
	allow_warnings();

	Scanner scanner{
	    "/home/djcb/Maildir",
	    [](const dirent* dentry) -> bool {
		    g_print("%02x %s\n", dentry->d_type, dentry->d_name);
		    return true;
	    },
	    [](const std::string& fullpath, const struct stat* statbuf, auto&& info) -> bool {
		    g_print("%s %zu\n", fullpath.c_str(), statbuf->st_size);
		    return true;
	    }};
	g_assert_true(scanner.start());

	while (scanner.is_running()) {
		sleep(1);
	}
}

int
main(int argc, char* argv[])
try {
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/utils/scanner/scan-maildir", test_scan_maildir);

	return g_test_run();

} catch (const std::runtime_error& re) {
	std::cerr << re.what() << "\n";
	return 1;
}
