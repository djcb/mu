/*
**  Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string>
#include <iostream>
#include "parser.hh"

int
main (int argc, char *argv[])
{
	std::string s;

	for (auto i = 1; i < argc; ++i)
		s += " " + std::string(argv[i]);

	Mu::WarningVec warnings;

	const auto tree = Mu::parse (s, warnings);
	for (const auto& w: warnings)
		std::cerr << "1:" << w.pos << ": " << w.msg << std::endl;

	std::cout << tree << std::endl;

	return 0;
}
