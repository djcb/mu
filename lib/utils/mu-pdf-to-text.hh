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

#ifndef MU_PDF_TO_TEXT_HH
#define MU_PDF_TO_TEXT_HH

#include <string>
#include <cstdint>
#include <span>

#include "mu-option.hh"

namespace Mu {
/**
 * Crudely convert PDF to plain text. This attempts to scrape the
 * human-readable text from the PDF so we can use it for indexing.
 *
 * In-process version, for internal use.
 *
 * @param data span with PDF data
 * @param max_size the maximum size of the text; any text beyond
 * that is dropped. 0 means unlimited.
 *
 * @return text extracted from the PDF, or Nothing
 */
[[nodiscard]] Option<std::string> pdf_to_text_internal(std::span<const uint8_t> data,
						       size_t max_size = 0);
}

#endif /*MU_PDF_TO_TEXT_HH*/
