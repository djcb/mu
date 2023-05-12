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
#include "config.h"
#include "mu-lang-detector.hh"

using namespace Mu;

#ifndef HAVE_CLD2
// Dummy implementation
Option<Language> Mu::detect_language(const std::string& txt) { return Nothing; }
#else
#include <cld2/public/compact_lang_det.h>
#include <cld2/public/encodings.h>

Option<Language>
Mu::detect_language(const std::string& txt)
{
	bool	is_reliable;
	const auto lang = CLD2::DetectLanguage(
		txt.c_str(), txt.length(),
		true/*plain-text*/,
		&is_reliable);

	if (lang == CLD2::UNKNOWN_LANGUAGE || !is_reliable)
		return {};

	Mu::Language res = {
		CLD2::LanguageName(lang),
		CLD2::LanguageCode(lang)
	};
	if (!res.name || !res.code)
		return {};
	else
		return Some(std::move(res));
}
#endif /*HAVE_CLD2*/

#ifdef BUILD_TESTS
#include <vector>
#include "mu-test-utils.hh"

static void
test_lang_detector()
{
	using	Case  = std::tuple<std::string,std::string, std::string>;
	using	Cases = std::vector<Case>;

	const Cases tests = {{
			{ "hello world, this is a bit of English",
			  "ENGLISH", "en" },
			{ "En nu een paar Nederlandse woorden",
			  "DUTCH", "nl" },
			{ "Hyvää huomenta! Puhun vähän suomea",
			  "FINNISH", "fi" },
			{ "So eine Arbeit wird eigentlich nie fertig, man muß sie für "
			  "fertig erklären, wenn man nach Zeit und Umständen das "
			  "möglichste getan hat.",
			  "GERMAN", "de"}
		}};

	for (auto&& test: tests) {
		const auto res = detect_language(std::get<0>(test));
#ifndef HAVE_CLD2
		g_assert_false(!!res);
#else
		g_assert_true(!!res);
		assert_equal(std::get<1>(test), res->name);
		assert_equal(std::get<2>(test), res->code);
#endif

	}
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/utils/lang-detector", test_lang_detector);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
