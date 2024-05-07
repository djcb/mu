/*
** Copyright (C) 2020-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-logger.hh"

#define G_LOG_USE_STRUCTURED
#include <glib.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <iostream>
#include <fstream>
#include <cstring>

#include <thread>
#include <mutex>

using namespace Mu;

static bool			MuLogInitialized = false;
static Mu::Logger::Options	MuLogOptions;
static std::ofstream		MuStream;
static auto			MaxLogFileSize	 = 1000 * 1024;
static std::mutex		logger_mtx;

static std::string MuLogPath;

static bool
maybe_open_logfile()
{
	if (MuStream.is_open())
		return true;

	const auto logdir{to_string_gchar(g_path_get_dirname(MuLogPath.c_str()))};
	if (g_mkdir_with_parents(logdir.c_str(), 0700) != 0) {
		mu_printerrln("creating {} failed: {}", logdir, g_strerror(errno));
		return false;
	}

	MuStream.open(MuLogPath, std::ios::out | std::ios::app);
	if (!MuStream.is_open()) {
		mu_printerrln("opening {} failed: {}", MuLogPath, g_strerror(errno));
		return false;
	}

	MuStream.sync_with_stdio(false);
	return true;
}

static bool
maybe_rotate_logfile()
{
	static unsigned n = 0;

	if (n++ % 1000 != 0)
		return true;

	GStatBuf statbuf;
	if (g_stat(MuLogPath.c_str(), &statbuf) == -1 || statbuf.st_size <= MaxLogFileSize)
		return true;

	const auto old = MuLogPath + ".old";
	g_unlink(old.c_str()); // opportunistic

	if (MuStream.is_open())
		MuStream.close();

	if (g_rename(MuLogPath.c_str(), old.c_str()) != 0)
		mu_printerrln("failed to rename {} -> {}: {}", MuLogPath, old, g_strerror(errno));

	return maybe_open_logfile();
}

static GLogWriterOutput
log_file(GLogLevelFlags level, const GLogField* fields, gsize n_fields, gpointer user_data)
{
	std::lock_guard lock{logger_mtx};

	if (!maybe_open_logfile())
		return G_LOG_WRITER_UNHANDLED;

	char   timebuf[22];
	time_t now{::time(NULL)};
	::strftime(timebuf, sizeof(timebuf), "%F %T", ::localtime(&now));

	char* msg = g_log_writer_format_fields(level, fields, n_fields, FALSE);
	if (msg && msg[0] == '\n') // hmm... seems lines start with '\n'r
		msg[0] = ' ';

	MuStream << timebuf << ' ' << msg << std::endl;

	g_free(msg);

	return maybe_rotate_logfile() ? G_LOG_WRITER_HANDLED : G_LOG_WRITER_UNHANDLED;
}

static GLogWriterOutput
log_stdouterr(GLogLevelFlags level, const GLogField* fields, gsize n_fields, gpointer user_data)
{
	return g_log_writer_standard_streams(level, fields, n_fields, user_data);
}



// log to some logging system; the one that is available & works of journal,
// syslog, file.
static GLogWriterOutput
log_system(GLogLevelFlags level, const GLogField* fields, gsize n_fields, gpointer user_data)
{
	GLogWriterOutput res = G_LOG_WRITER_UNHANDLED;

#ifdef MAYBE_USE_JOURNAL
	res = g_log_writer_journald(level, fields, n_fields, user_data);
	if (res == G_LOG_WRITER_HANDLED)
		return res;
#endif /*MAYBE_USE_JOURNAL*/

#ifdef MAYBE_USE_SYSLOG
	/* since glib 2.80 */
	res = g_log_writer_syslog(level, fields, n_fields, user_data);
	if (res == G_LOG_WRITER_HANDLED)
		return res;
#endif /*MAYBE_USE_SYSLOG*/

	return res = log_file(level, fields, n_fields, user_data);
}


Result<Logger>
Mu::Logger::make(const std::string& path, Mu::Logger::Options opts)
{
	if (MuLogInitialized)
		return Err(Error::Code::Internal, "logging already initialized");

	return Ok(Logger(path, opts));
}

Mu::Logger::Logger(const std::string& path, Mu::Logger::Options opts)
{
	if (g_getenv("MU_LOG_STDOUTERR"))
		opts |= Logger::Options::StdOutErr;

	MuLogOptions = opts;
	MuLogPath    = path;

	g_log_set_writer_func(
	    [](GLogLevelFlags level, const GLogField* fields, gsize n_fields, gpointer user_data) {
		    // filter out debug-level messages?
		    if (level == G_LOG_LEVEL_DEBUG &&
			(none_of(MuLogOptions & Options::Debug)))
			    return G_LOG_WRITER_HANDLED;

		    // log criticals to stdout / err or if asked
		    if (level == G_LOG_LEVEL_CRITICAL ||
			any_of(MuLogOptions & Options::StdOutErr)) {
			    log_stdouterr(level, fields, n_fields, user_data);
		    }

		    // log to the journal, or, if not available to a file.
		    if (any_of(MuLogOptions & Options::File))
			return log_file(level, fields, n_fields, user_data);

		    return log_system(level, fields, n_fields, user_data);
	    },
	    NULL,
	    NULL);

	g_message("logging initialized; debug: %s, stdout/stderr: %s",
		  any_of(opts & Options::Debug) ? "yes" : "no",
		  any_of(opts & Options::StdOutErr) ? "yes" : "no");

	MuLogInitialized = true;
}

Logger::~Logger()
{
	if (!MuLogInitialized)
		return;

	if (MuStream.is_open())
		MuStream.close();

	MuLogInitialized = false;
}


#ifdef BUILD_TESTS
#include <vector>
#include <atomic>

#include "mu-test-utils.hh"
#include "mu-utils-file.hh"

static void
test_logger_threads(void)
{
	TempDir temp_dir;
	const auto testpath{join_paths(temp_dir.path(), "test.log")};
	mu_message("log-file: {}", testpath);

	auto logger = Logger::make(testpath, Logger::Options::File | Logger::Options::Debug);
	assert_valid_result(logger);

	const auto		thread_num = 16;
	std::atomic<bool>	running	   = true;

	std::vector<std::thread> threads;

	/* log to the logger file from many threass */
	for (auto n = 0; n != thread_num; ++n)
		threads.emplace_back(
			std::thread([&running]{
				while (running) {
					//mu_debug("log message from thread <{}>", n);
					std::this_thread::yield();
				}
			}));

	using namespace std::chrono_literals;
	std::this_thread::sleep_for(1s);
	running = false;

	for (auto n = 0; n != 16; ++n)
		if (threads[n].joinable())
			threads[n].join();
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/utils/logger", test_logger_threads);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
