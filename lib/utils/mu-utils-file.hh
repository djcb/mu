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

#ifndef MU_UTILS_FILE_HH__
#define MU_UTILS_FILE_HH__

#include <string>
#include <cinttypes>
#include <sys/stat.h>

#include <gio/gio.h>
#include <utils/mu-option.hh>
#include <utils/mu-result.hh>
#include <utils/mu-regex.hh>

namespace Mu {

/**
 * Check if the directory has the given attributes
 *
 * @param path path to dir
 * @param readable is it readable? false means "don't care"
 * @param writeable is it writable? false means "don't care"
 *
 * @return true if is is a directory with given attributes; false otherwise.
 */
bool check_dir(const std::string& path, bool readable=false, bool writeable=false);

/**
 * See g_canonicalize_filename
 *
 * @param filename
 * @param relative_to
 *
 * @return
 */
std::string canonicalize_filename(const std::string& path, const std::string& relative_to="");

/**
 * Expand the filesystem path (as per wordexp(3))
 *
 * @param str a filesystem path string
 *
 * @return the expanded string or some error
 */
Result<std::string> expand_path(const std::string& str);


/**
 * Get the basename for path, i.e. without leading directory component,
 * @see g_path_get_basename
 *
 * @param path
 *
 * @return the basename
 */
std::string basename(const std::string& path);


/**
 * Get the dirname for path, i.e. without leading directory component,
 * @see g_path_get_dirname
 *
 * @param path
 *
 * @return the dirname
 */
std::string dirname(const std::string& path);


/*
 * for OSs without support for direntry->d_type, like Solaris
 */
#ifndef DT_UNKNOWN
enum {
	DT_UNKNOWN = 0,
#define DT_UNKNOWN	DT_UNKNOWN
	DT_FIFO	   = 1,
#define DT_FIFO		DT_FIFO
	DT_CHR	   = 2,
#define DT_CHR		DT_CHR
	DT_DIR	   = 4,
#define DT_DIR		DT_DIR
	DT_BLK	   = 6,
#define DT_BLK		DT_BLK
	DT_REG	   = 8,
#define DT_REG		DT_REG
	DT_LNK	   = 10,
#define DT_LNK		DT_LNK
	DT_SOCK	   = 12,
#define DT_SOCK		DT_SOCK
	DT_WHT	   = 14
#define DT_WHT		DT_WHT
};
#endif /*DT_UNKNOWN*/

 /**
 * get the d_type (as in direntry->d_type) for the file at path, using either
 * stat(3) or lstat(3)
 *
 * @param path full path
 * @param use_lstat whether to use lstat (otherwise use stat)
 *
 * @return DT_REG, DT_DIR, DT_LNK, or DT_UNKNOWN (other values are not supported
 * currently)
 */
uint8_t determine_dtype(const std::string& path, bool use_lstat=false);


/**
 * Well-known runtime paths
 *
 */
enum struct RuntimePath {
	XapianDb,
	Cache,
	LogFile,
	Config,
	Scripts,
	Bookmarks
};

/**
 * Get some well-known Path for internal use when don't have
 * access to the command-line
 *
 * @param path the RuntimePath to find
 * @param muhome path to muhome directory, or empty for the default.
 *
 * @return the path name
 */
std::string runtime_path(RuntimePath path, const std::string& muhome="");

/**
 * Join path components into a path (with '/')
 *
 * @param s a string-convertible value
 * @param args 0 or more string-convertible values
 *
 * @return the path
 */
static inline std::string join_paths() { return {}; }
template<typename S> std::string join_paths_(S&& s) { return std::string{s}; }
template<typename	S, typename...Args>
std::string join_paths_(S&& s, Args...args) {

	static std::string sepa{"/"};
	auto&& str{std::string{std::forward<S>(s)}};
	if (auto&& rest{join_paths_(std::forward<Args>(args)...)}; !rest.empty())
		str += (sepa + rest);
	return str;
}

template<typename S, typename...Args>
std::string join_paths(S&& s, Args...args) {

	constexpr auto sepa = '/';
	auto path = join_paths_(std::forward<S>(s), std::forward<Args>(args)...);

	auto c{0U};
	while (c < path.size()) {

		if (path[c] != sepa) {
			++c;
			continue;
		}

		while (path[++c] == '/') {
			path.erase(c, 1);
			--c;
		}
	}

	return path;
}


/**
 * Like g_cancellable_new(), but automatically cancels itself
 * after timeout
 *
 * @param timeout timeout in millisecs
 *
 * @return A GCancellable* instances; free with g_object_unref() when
 * no longer needed.
 */
GCancellable* g_cancellable_new_with_timeout(guint timeout);

/**
 * Read for standard input
 *
 * @return data from standard input or an error.
 */
Result<std::string> read_from_stdin();

/**
 * Create a randomly-named temporary directory
 *
 * @return name of the temporary directory or an error.
 */
Result<std::string> make_temp_dir();


/**
 * Remove a directory, recursively. Does not have to be empty.
 *
 * @param path path to directory
 *
 * @return Ok() or an error.
 */
Result<void> remove_directory(const std::string& path);

/**
 * Run some system command.
 *
 * @param args a list of commmand line arguments (like argv)
 * @param try_setsid whether to try setsid(2) (see its manpage for details) if this
 *        system supports it.
 *
 * @return Ok(exit code) or an error. Note that exit-code != 0 is _not_
 * considered an error from the perspective of run_command, but is for
 * run_command0
 */
struct CommandOutput {
	int exit_code;
	std::string standard_out;
	std::string standard_err;
};
Result<CommandOutput> run_command(std::initializer_list<std::string> args,
				  bool try_setsid=false);
Result<CommandOutput> run_command0(std::initializer_list<std::string> args,
				   bool try_setsid=false);

/**
 * Try to 'play' (ie., open with it's associated program) a file. On MacOS, the
 * the program 'open' is used for this; on other platforms 'xdg-open' to do the
 * actual opening. In addition you can set it to another program by setting thep
 * MU_PLAY_PROGRAM environment variable
 *
 * This requires a 'native' file, see g_file_is_native()
 *
 * @param path full path of the file to open
 *
 * @return Ok() if succeeded, some error otherwise.
 */
Result<void> play(const std::string& path);

/**
 * Find program in PATH
 *
 * @param name the name of the program
 *
 * @return either the full path to program, or Nothing if not found.
 */
Option<std::string> program_in_path(const std::string& name);

} // namespace Mu

#endif /* MU_UTILS_FILE_HH__ */
