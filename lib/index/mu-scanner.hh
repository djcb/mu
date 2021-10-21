/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_SCANNER_HH__
#define MU_SCANNER_HH__

#include <functional>
#include <memory>

#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

namespace Mu {

/// @brief Maildir scanner
///
/// Scans maildir (trees) recursively, and calls the Handler callback for
/// directories & files.
///
/// It filters out (i.e., does *not* call the handler for):
///  - files starting with '.'
///  - files that do not live in a cur / new leaf maildir
///  - directories '.' and '..' and 'tmp'
///
class Scanner {
	public:
	enum struct HandleType {
		File,
		EnterNewCur, /* cur/ or new/ */
		EnterDir,    /* some other directory */
		LeaveDir
	};

	/// Prototype for a handler function
	using Handler = std::function<
	    bool(const std::string& fullpath, struct stat* statbuf, HandleType htype)>;
	/**
	 * Construct a scanner object for scanning a directory, recursively.
	 *
	 * If handler is a directory
	 *
	 *
	 * @param root_dir root dir to start scanning
	 * @param handler handler function for some direntry
	 */
	Scanner(const std::string& root_dir, Handler handler);

	/**
	 * DTOR
	 */
	~Scanner();

	/**
	 * Start the scan; this is a blocking call than runs until
	 * finished or (from another thread) stop() is called.
	 *
	 * @return true if starting worked; false otherwise
	 */
	bool start();

	/**
	 * Stop the scan
	 *
	 * @return true if stopping worked; false otherwi%sse
	 */
	bool stop();

	/**
	 * Is a scan currently running?
	 *
	 * @return true or false
	 */
	bool is_running() const;

	private:
	struct Private;
	std::unique_ptr<Private> priv_;
};

} // namespace Mu

#endif /* MU_SCANNER_HH__ */
