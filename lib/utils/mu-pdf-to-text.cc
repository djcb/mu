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

#include "mu-utils.hh"
#include "mu-option.hh"

#include "mu-pdf-to-text.hh"

using namespace Mu;

#if !BUILD_PDF_TO_TEXT
Option<std::string>
Mu::pdf_to_text_internal(const std::vector<uint8_t>& data)
{
	return Nothing;
}
#else
#include <poppler.h>
#include <glib.h>

#include "mu-utils-file.hh"

// Note, this the in-process pdf_to_text; it is simple and it works, but not
// good for real-world untrusted pdfs since poppler may have bugs.
Option<std::string>
Mu::pdf_to_text_internal(const std::vector<uint8_t>& data)
{
	GBytes* bytes{g_bytes_new_static(data.data(), data.size())};
	GError *err{};
	PopplerDocument* doc{poppler_document_new_from_bytes(bytes, NULL, &err)};
	g_bytes_unref(bytes);

	if (!doc) {
		// warnings are common, e.g. for encrypted/borked pdfs
		mu_debug("failed to create poppler document: {}",
			 err ? err->message : "something went awry");
		g_clear_error(&err);
		return Nothing;
	}

	GString *gstr = g_string_new(nullptr);
	const auto pages{poppler_document_get_n_pages(doc)};
	for (int p = 0; p < pages; ++p) {

		PopplerPage *page{poppler_document_get_page(doc, p)};
		if (!page)
			continue;

		if (char *text{poppler_page_get_text(page)}; text) {
			g_string_append(gstr, text);
			g_string_append_c(gstr, '\n');
			g_free(text);
		}
		g_object_unref(page);
	}

	g_object_unref(doc);

	std::string result{gstr->str};
	g_string_free(gstr, TRUE);

	return result;
}

#endif /*BUILD_PDF_TO_TEXT*/

#ifdef BUILD_TESTS
#include "mu-test-utils.hh"
#include "mu-utils-file.hh"

static void
test_pdf_to_text_internal()
{
#if !BUILD_PDF_TO_TEXT
	g_test_skip("pdf_to_text not available");
	return;
#endif /*BUILD_PDF_TO_TEXT*/

	const auto pdf{read_from_file(join_paths(MU_TESTDATADIR, "misc", "pdftest.pdf"))};
	assert_ok(pdf);

	// test in-process, without the sandbox
	const auto txt{pdf_to_text_internal(to_bytes(*pdf))};
	g_assert_true(txt.has_value());

	g_assert_true(txt->find("hagelslag") != std::string::npos);
	g_assert_true(txt->find("stroopwafel") != std::string::npos);
	g_assert_true(txt->find("hopjesvla") == std::string::npos);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/pdf-to-text/pdf-to-text-internal",
			test_pdf_to_text_internal);

	return g_test_run();
}
#endif // BUILD_TESTS
