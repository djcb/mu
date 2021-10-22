/*
**  Copyright (C) 2017-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-tokenizer.hh"

int
main(int argc, char* argv[])
{
	std::string s;

	for (auto i = 1; i < argc; ++i)
		s += " " + std::string(argv[i]);

	const auto tvec = Mu::tokenize(s);
	for (const auto& t : tvec)
		std::cout << t << std::endl;

	return 0;
}
