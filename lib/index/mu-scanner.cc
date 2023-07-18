/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-scanner.hh"

#include "config.h"

#include <chrono>
#include <mutex>
#include <atomic>
#include <thread>
#include <cstring>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>

#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include "utils/mu-error.hh"

using namespace Mu;

struct Scanner::Private {
	Private(const std::string& root_dir, Scanner::Handler handler)
	    : root_dir_{root_dir}, handler_{handler}
	{
		if (!handler_)
			throw Mu::Error{Error::Code::Internal, "missing handler"};
	}
	~Private() { stop(); }

	bool start();
	bool stop();
	bool process_dentry(const std::string& path, struct dirent* dentry, bool is_maildir);
	bool process_dir(const std::string& path, bool is_maildir);

	const std::string      root_dir_;
	const Scanner::Handler handler_;
	std::atomic<bool>      running_{};
	std::mutex             lock_;
};

static bool
is_dotdir(const char *d_name)
{
	/* dotdir? */
	if (d_name[0] == '\0' || (d_name[1] == '\0' && d_name[0] == '.') ||
	    (d_name[2] == '\0' && d_name[0] == '.' && d_name[1] == '.'))
		return true;

	return false;
}

static bool
do_ignore(const char *d_name)
{
	if (d_name[0] == '.') {
		if (d_name[1] == '#') /* emacs? */
			return true;
		if (g_strcmp0(d_name + 1, "nnmaildir") == 0) /* gnus? */
			return true;
		if (g_strcmp0(d_name + 1, "notmuch") == 0) /* notmuch? */
			return true;
	}

	if (g_strcmp0(d_name, "hcache.db") == 0) /* mutt cache? */
		return true;

	return false;
}

bool
Scanner::Private::process_dentry(const std::string& path, struct dirent *dentry,
				 bool is_maildir)
{
	const auto d_name{dentry->d_name};

	if (is_dotdir(d_name) || std::strcmp(d_name, "tmp") == 0)
		return true; // ignore.
	if (do_ignore(d_name)) {
		mu_debug("skip {}/{} (ignore)", path, d_name);
		return true; // ignore
	}

	const auto  fullpath{join_paths(path, d_name)};
	struct stat statbuf {};
	if (::stat(fullpath.c_str(), &statbuf) != 0) {
		mu_warning("failed to stat {}: {}", fullpath, g_strerror(errno));
		return false;
	}

	if (S_ISDIR(statbuf.st_mode)) {
		const auto new_cur =
		    std::strcmp(d_name, "cur") == 0 || std::strcmp(d_name, "new") == 0;
		const auto htype =
		    new_cur ? Scanner::HandleType::EnterNewCur : Scanner::HandleType::EnterDir;
		const auto res = handler_(fullpath, &statbuf, htype);
		if (!res)
			return true; // skip

		process_dir(fullpath, new_cur);

		return handler_(fullpath, &statbuf, Scanner::HandleType::LeaveDir);

	} else if (S_ISREG(statbuf.st_mode) && is_maildir)
		return handler_(fullpath, &statbuf, Scanner::HandleType::File);

	mu_debug("skip {} (neither maildir-file nor directory)", fullpath);

	return true;
}

bool
Scanner::Private::process_dir(const std::string& path, bool is_maildir)
{
	if (!running_)
		return true; /* we're done */

	if (G_UNLIKELY(path.length() > PATH_MAX)) {
		// note: unlikely to hit this, one case would be a
		// self-referential symlink; that should be caught earlier,
		// so this is just a backstop.
		mu_warning("path is too long: {}", path);
		return false;
	}

	const auto dir{::opendir(path.c_str())};
	if (G_UNLIKELY(!dir)) {
		mu_warning("failed to scan dir {}: {}", path, g_strerror(errno));
		return false;
	}

	// TODO: sort dentries by inode order, which makes things faster for extfs.
	// see mu-maildir.c

	while (running_) {
		errno = 0;
		const auto dentry{::readdir(dir)};

		if (G_LIKELY(dentry)) {
			process_dentry(path, dentry, is_maildir);
			continue;
		}

		if (errno != 0) {
			mu_warning("failed to read {}: {}", path, g_strerror(errno));
			continue;
		}

		break;
	}
	closedir(dir);

	return true;
}

bool
Scanner::Private::start()
{
	const auto& path{root_dir_};
	if (G_UNLIKELY(path.length() > PATH_MAX)) {
		mu_warning("path is too long: {}", path);
		return false;
	}

	const auto mode{F_OK | R_OK};
	if (G_UNLIKELY(access(path.c_str(), mode) != 0)) {
		mu_warning("'{}' is not readable: {}", path, g_strerror(errno));
		return false;
	}

	struct stat statbuf {};
	if (G_UNLIKELY(stat(path.c_str(), &statbuf) != 0)) {
		mu_warning("'{}' is not stat'able: {}", path, g_strerror(errno));
		return false;
	}

	if (G_UNLIKELY(!S_ISDIR(statbuf.st_mode))) {
		mu_warning("'{}' is not a directory", path);
		return false;
	}

	running_ = true;
	mu_debug("starting scan @ {}", root_dir_);

	auto       basename{g_path_get_basename(root_dir_.c_str())};
	const auto is_maildir =
	    (g_strcmp0(basename, "cur") == 0 || g_strcmp0(basename, "new") == 0);
	g_free(basename);

	const auto start{std::chrono::steady_clock::now()};
	process_dir(root_dir_, is_maildir);
	const auto elapsed = std::chrono::steady_clock::now() - start;
	mu_debug("finished scan of {} in {} ms", root_dir_, to_ms(elapsed));
	running_ = false;

	return true;
}

bool
Scanner::Private::stop()
{
	if (!running_)
		return true; // nothing to do

	mu_debug("stopping scan");
	running_ = false;

	return true;
}

Scanner::Scanner(const std::string& root_dir, Scanner::Handler handler)
    : priv_{std::make_unique<Private>(root_dir, handler)}
{
}

Scanner::~Scanner() = default;

bool
Scanner::start()
{
	if (priv_->running_)
		return true; // nothing to do

	const auto res  = priv_->start(); /* blocks */
	priv_->running_ = false;

	return res;
}

bool
Scanner::stop()
{
	std::lock_guard l(priv_->lock_);

	return priv_->stop();
}

bool
Scanner::is_running() const
{
	return priv_->running_;
}




#if BUILD_TESTS
#include "mu-test-utils.hh"


static void
test_scan_maildir()
{
	allow_warnings();

	size_t count{};
	Scanner scanner{
		MU_TESTMAILDIR,
		[&](const std::string& fullpath, const struct stat* statbuf, auto&& htype) -> bool {
			mu_debug("{} {}", fullpath, statbuf->st_size);
			++count;
			return true;
		}};
	g_assert_true(scanner.start());

	while (scanner.is_running()) { g_usleep(1000); }

	// very rudimentary test...
	g_assert_cmpuint(count,==,23);
}

int
main(int argc, char* argv[])
try {
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/index/scanner/scan-maildir", test_scan_maildir);

	return g_test_run();

} catch (const std::runtime_error& re) {
	mu_printerrln("caught runtime error: {}", re.what());
	return 1;
} catch (...) {
	mu_printerrln("caught exception");
	return 1;
}


#endif /*BUILD_TESTS*/
