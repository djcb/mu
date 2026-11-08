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

#include <atomic>
#include <unistd.h>
#include <glib.h>

#include "mu-option.hh"
#include "mu-pdf-to-text.hh"
#include "mu-utils-file.hh"

// Note: this file is only compiled when building with pdf-to-text support (see
// meson.build); without it, there's a dummy pdf_to_text in mu-utils.cc.
//
// Only the mu-pdf2text helper and the unit-tests should link with poppler, not
// mu itself; so the poppler-using part (pdf_to_text_internal) is only compiled
// with BUILD_PDF_TO_TEXT_INTERNAL, and is not part of lib-mu-utils.

using namespace Mu;

#ifdef BUILD_PDF_TO_TEXT_INTERNAL
#include <poppler.h>

// Note, this is the in-process pdf_to_text; it is simple and it works, but not
// good for real-world untrusted pdfs since poppler may have bugs, so for those,
// use pdf_to_text which runs this in the locked-down mu-pdf2text helper in a
// separate process.
Option<std::string>
Mu::pdf_to_text_internal(std::span<const uint8_t> data, size_t max_size)
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

		if (max_size != 0 && gstr->len >= max_size)
			break; // enough is enough

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

	if (max_size != 0 && gstr->len > max_size)
		g_string_truncate(gstr, max_size);

	return to_string_gchar(g_string_free_and_steal(gstr));
}
#endif /*BUILD_PDF_TO_TEXT_INTERNAL*/

Option<std::string>
Mu::pdf_to_text(std::span<const uint8_t> data)
{
	static std::atomic<bool> prog_not_found;

	if (prog_not_found)
		return Nothing;

	if (data.size() > max_pdf_size) {
		mu_debug("skipping too-big pdf ({} bytes)", data.size());
		return Nothing;
	}

	// environment override? note that this is just for
	// dev/debugging, not for user-overrides.
	const char *prog{g_getenv("MU_PDF_TO_TEXT_PROGRAM")};
	if (!prog || !*prog)
		prog = MU_PDF_TO_TEXT_PROGRAM;

	// The helper reads all of stdin, then writes to stdout. It should limit
	// its output-size, but double-check here. Time-limit is with
	// lockdown_process
	constexpr auto   timeout_s{"5"};      // time-limit for the helper
	constexpr size_t max_text{2 << 20};   // maximum text-size (bytes)
	static const auto max_text_str{mu_format("{}", max_text)};

	if (auto&& res{read_from_helper({prog, "--timeout", timeout_s,
					"--max-output", max_text_str},
			data, max_text)}; !res) {
		// failures are not uncommon, e.g. for encrypted/b0rked PDFs so
		// let's only log at debug level, to avoid log spam.
		mu_debug("pdf-to-text failed: {}", res.error().what());
		// perhaps this due to missing tool? Warn only once about that.
		// avoid log spam.
		if (::access(prog, X_OK) != 0) {
			mu_warning("cannot find executable {}; pdf extraction disabled", prog);
			prog_not_found = true;
		}
		return Nothing;
	} else {
		mu_debug("{} bytes of pdf -> {} bytes of text ", data.size(), res->size());
		return std::move(*res);
	}
}

#ifdef BUILD_TESTS
#include "mu-test-utils.hh"

static void
test_pdf_to_text_internal()
{
	const auto pdf{read_from_file(join_paths(MU_TESTDATADIR, "misc", "pdftest.pdf"))};
	assert_ok(pdf);

	// test in-process, without the sandbox
	const auto txt{pdf_to_text_internal(to_bytes(*pdf))};
	g_assert_true(txt.has_value());

	g_assert_true(txt->find("hagelslag") != std::string::npos);
	g_assert_true(txt->find("stroopwafel") != std::string::npos);
	g_assert_true(txt->find("hopjesvla") == std::string::npos);

	// limited size
	const auto txt5{pdf_to_text_internal(to_bytes(*pdf), 5)};
	g_assert_true(txt5.has_value());
	assert_equal(*txt5, txt->substr(0, 5));
}

static void
test_pdf_to_text_external()
{
	const auto pdf{read_from_file(join_paths(MU_TESTDATADIR, "misc", "pdftest.pdf"))};
	assert_ok(pdf);

	const auto txt{pdf_to_text(to_bytes(*pdf))};
	g_assert_true(txt.has_value());
	g_assert_true(txt->find("hagelslag") != std::string::npos);
	g_assert_true(txt->find("stroopwafel") != std::string::npos);
	g_assert_true(txt->find("hopjesvla") == std::string::npos);

	// not a pdf; this logs a warning
	allow_warnings();
	const std::string_view junk{"this is not a pdf"};
	g_assert_false(!!pdf_to_text({reinterpret_cast<const uint8_t*>(junk.data()), junk.size()}));

	// logging in the locked-down helper should not get it killed; i.e., it
	// should exit normally (with an error), even with debug-logging enabled.
	const char *prog{g_getenv("MU_PDF_TO_TEXT_PROGRAM")};
	if (!prog || !*prog)
		prog = MU_PDF_TO_TEXT_PROGRAM;
	g_setenv("G_MESSAGES_DEBUG", "all", TRUE);
	const auto res{read_from_helper({prog, "--timeout", "5"}, junk)};
	g_unsetenv("G_MESSAGES_DEBUG");
	g_assert_false(!!res);
	g_assert_true(std::string_view{res.error().what()}.find("returned 1") !=
		      std::string_view::npos);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/pdf-to-text/pdf-to-text-internal",
			test_pdf_to_text_internal);
	g_test_add_func("/pdf-to-text/pdf-to-text-external",
			test_pdf_to_text_external);

	return g_test_run();
}
#endif // BUILD_TESTS
