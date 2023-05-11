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

#ifndef MU_LANG_DETECTOR_HH__
#define MU_LANG_DETECTOR_HH__

#include <string>
#include "mu-option.hh"

namespace Mu {

struct Language {
	const char *name;	/**< Language name, e.g. "Dutch" */
	const char *code;	/**< Language code, e.g. "nl" */
};

/**
 * Detect the language of text
 *
 * @param txt some text (UTF-8)
 *
 * @return either a Language or nothing; the latter
 * also if we cannot not reliably determine a single language
 */
Option<Language> detect_language(const std::string& txt);

} // namespace Mu


#endif /* MU_LANG_DETECTOR_HH__ */
