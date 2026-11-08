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
#include "config.h"

#include <cstdio>
#include <cstdlib>
#include <exception>
#include <unistd.h>
#include <array>
#include <cstdint>
#include <span>

#include <poppler.h>

#include "mu-lockdown.hh"
#include "mu-utils.hh"
#include "mu-utils-file.hh"
#include "mu-pdf-to-text.hh"

#include <CLI/CLI.hpp>

// a tiny PDF for warming up poppler _before_ locking it down
// attempts to load fonts etc.
#include "poppler-warmup.pdf.inc"

using namespace Mu;

/**
 * Make poppler initialize its global state and load its data-files (including
 * the cidToUnicode-maps), so this does not need to happen after lock-down.
 *
 * Poppler keeps this state only while at least one document exists, so the
 * returned document must be kept alive while using poppler.
 *
 * @return a (dummy) poppler document, or nullptr in case of error
 */
static PopplerDocument*
warm_up_poppler()
{
	GBytes* bytes{g_bytes_new_static(warm_up_pdf.data(), warm_up_pdf.size())};
	PopplerDocument* doc{poppler_document_new_from_bytes(bytes, {}, {})};
	g_bytes_unref(bytes);
	if (!doc)
		return {};

	PopplerPage *page{poppler_document_get_page(doc, 0)};
	if (!page) {
		g_object_unref(doc);
		return {};
	}
	g_free(poppler_page_get_text(page));
	g_object_unref(page);

	return doc;
}


static int
locked_down_pdf_to_text(const std::string& pdf, size_t timeout, size_t max_output) try {
	// seems poppler's C api may throw(!)

	if (PopplerDocument* doc{warm_up_poppler()}; !doc) {
		mu_printerrln("error: failed to initialize poppler");
		return 1;
	}

	lockdown_process(STDOUT_FILENO, timeout);

	const std::span data{reinterpret_cast<const uint8_t*>(pdf.data()), pdf.size()};
	if (const auto txt{pdf_to_text_internal(data, max_output)}; !txt) {
		mu_printerrln("conversion failed");
		return 1;
	} else {
		mu_print("{}", *txt);
		return 0;
	}
} catch (const std::exception& ex) {
	mu_printerrln("conversion failed: {}", ex.what());
	return 1;
}

// sandboxed tool that reads a PDF from a path or stdin and prints text on
// stdout.
//
// For example:
//    mu-pdf2text /tmp/bla.pdf
// or
//   curl --silent https://www.example.com/bla.pdf | mu-pdf2text --timeout 5
int
main (int argc, char *argv[])
{
	size_t timeout{}, max_output{};
	std::string path;

	CLI::App app;
	app.description(mu_format(R"(mu-pdf2text - convert PDF to text

		Copyright (C) {} Dirk-Jan C. Binnema

License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.)", MU_COPYRIGHT_YEAR));

	app.add_option("-t,--timeout", timeout,
		       "Maximum cpu-time in seconds; wall-clock time is limited\n"
		       "to three times that (0: no limit)");
	app.add_option("-m,--max-output", max_output,
		       "Maximum size of the output in bytes (0: no limit)");
	app.add_option("file", path, "PDF file (default: standard input)");
	CLI11_PARSE(app, argc, argv);

	if (path == "-") // '-' means stdin
		path.clear();

	const auto res = path.empty() ? read_from_stdin() : read_from_file(path);
	if (!res) {
		mu_printerrln("error: {}", res.error().what());
		return 1;
	}

	return locked_down_pdf_to_text(*res, timeout, max_output);
}
