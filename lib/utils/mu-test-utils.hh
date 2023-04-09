/*
** Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_TEST_UTILS_HH__
#define MU_TEST_UTILS_HH__

#include <string>
#include <utils/mu-utils.hh>

namespace Mu {

/**
 * get a dir name for a random temporary directory to do tests
 *
 * @return a random dir name, g_free when it's no longer needed
 */
char* test_mu_common_get_random_tmpdir(void);
static inline std::string test_random_tmpdir() {
	return to_string_gchar(test_mu_common_get_random_tmpdir());
}

/**
 * mu wrapper for g_test_init. Sets environment variable MU_TEST to 1.
 *
 * @param argc
 * @param argv
 */
void mu_test_init(int *argc, char ***argv);

/**
 * set the timezone
 *
 * @param tz timezone
 *
 * @return the old timezone
 */
const char* set_tz(const char* tz);

/**
 * switch the locale to en_US.utf8, return TRUE if it succeeds
 *
 * @return true if the switch succeeds, false otherwise
 */
bool set_en_us_utf8_locale();

/**
 * For unit tests, assert two std::string's are equal.
 *
 * @param s1 string1
 * @param s2 string2
 */
#define assert_equal(s1__,s2__) do {					\
		std::string s1s__(s1__), s2s__(s2__);			\
		g_assert_cmpstr(s1s__.c_str(), ==, s2s__.c_str());	\
	} while(0)


#define assert_equal_seq(seq1__, seq2__) do {				\
		g_assert_cmpuint(seq1__.size(), ==, seq2__.size());	\
		size_t n__{};						\
		for (auto&& item__: seq1__) {				\
			g_assert_true(item__ == seq2__.at(n__));	\
			++n__;						\
		}							\
		} while(0)

#define assert_equal_seq_str(seq1__, seq2__) do {			\
		g_assert_cmpuint(seq1__.size(), ==, seq2__.size());	\
		size_t n__{};						\
		for (auto&& item__: seq1__) {				\
			assert_equal(item__, seq2__.at(n__));		\
			++n__;						\
		}							\
		} while(0)

/**
 * For unit-tests, allow warnings in the current function.
 *
 */
void allow_warnings();


/**
 * For unit-tests, a RAII tempdir.
 *
 */
struct TempDir {
	/**
	 * Construct a temporary directory
	 */
	TempDir(bool autodelete=true);

	/**
	 * DTOR; removes the temporary directory
	 *
	 *
	 * @return
	 */
	~TempDir();

	/**
	 * Path to the temporary directory
	 *
	 * @return the path.
	 *
	 *
	 */
	const std::string& path() {return path_; }
private:
	std::string path_;
	const bool autodelete_;
};

} // namepace Mu


#endif /* MU_TEST_UTILS_HH__ */
