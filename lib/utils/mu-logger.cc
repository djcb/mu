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

#define G_LOG_USE_STRUCTURED
#include <glib.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <iostream>
#include <fstream>
#include <cstring>

#include "mu-logger.hh"

using namespace Mu;

static bool           MuLogInitialized = false;
static Mu::LogOptions MuLogOptions;
static std::ofstream  MuStream;
static auto           MaxLogFileSize   = 1000 * 1024;

static std::string     MuLogPath;

static bool
maybe_open_logfile ()
{
        if (MuStream.is_open())
                return true;

        MuStream.open (MuLogPath, std::ios::out | std::ios::app );
        if (!MuStream.is_open()) {
                std::cerr << "opening " << MuLogPath << " failed:"
                          << strerror(errno) << std::endl;
                return false;
        }

        MuStream.sync_with_stdio(false);
        return true;
}

static bool
maybe_rotate_logfile ()
{
        static unsigned n = 0;

        if (n++ % 1000 != 0)
                return true;

        GStatBuf statbuf;
        if (::stat (MuLogPath.c_str(), &statbuf) == -1 ||
            statbuf.st_size <= MaxLogFileSize)
                return true;

        const auto old = MuLogPath + ".old";
        g_unlink(old.c_str()); // opportunistic

        if (MuStream.is_open())
                MuStream.close();

        if (g_rename(MuLogPath.c_str(), old.c_str()) != 0)
                std::cerr << "failed to rename "
                          << MuLogPath << " -> " << old.c_str()
                          << ": " << ::strerror(errno) << std::endl;

        return maybe_open_logfile();
}

static GLogWriterOutput
log_file (GLogLevelFlags level, const GLogField *fields, gsize n_fields,
          gpointer user_data)
{
        if (!maybe_open_logfile())
                return G_LOG_WRITER_UNHANDLED;

        char timebuf[22];
        time_t now{::time(NULL)};
        ::strftime (timebuf, sizeof(timebuf), "%F %T", ::localtime(&now));

        char *msg = g_log_writer_format_fields (level, fields, n_fields, FALSE);
        if (msg && msg[0] == '\n') // hmm... seems lines start with '\n'r
                msg[0] = ' ';

        MuStream << timebuf << ' ' << msg << std::endl;

        g_free (msg);

        return maybe_rotate_logfile() ? G_LOG_WRITER_HANDLED : G_LOG_WRITER_UNHANDLED;
}

static GLogWriterOutput
log_stdouterr (GLogLevelFlags level, const GLogField *fields, gsize n_fields,
               gpointer user_data)
{
        return g_log_writer_standard_streams (level, fields, n_fields, user_data);
}

static GLogWriterOutput
log_journal (GLogLevelFlags level, const GLogField *fields, gsize n_fields,
                     gpointer user_data)
{
        return g_log_writer_journald (level, fields, n_fields, user_data);
}

void
Mu::log_init (const std::string& path, Mu::LogOptions opts)
{
        if (MuLogInitialized) {
                g_error ("logging is already initialized");
                return;
        }

        MuLogOptions = opts;
        MuLogPath    = path;

        g_log_set_writer_func (
                [](GLogLevelFlags level, const GLogField *fields, gsize n_fields,
                   gpointer user_data) {

                        // filter out debug-level messages?
                        if (level == G_LOG_LEVEL_DEBUG &&
                            (none_of (MuLogOptions & Mu::LogOptions::Debug)))
                                return G_LOG_WRITER_HANDLED;

                        // log criticals to stdout / err or if asked
                        if (level == G_LOG_LEVEL_CRITICAL ||
                            any_of(MuLogOptions & Mu::LogOptions::StdOutErr)){
                                log_stdouterr (level, fields, n_fields, user_data);
                        }

                        // log to the journal, or, if not available to a file.
                        if (log_journal (level, fields, n_fields, user_data) !=
                            G_LOG_WRITER_HANDLED)
                                return log_file (level, fields, n_fields, user_data);
                        else
                                return G_LOG_WRITER_HANDLED;
                }, NULL, NULL);

        g_message ("logging initialized; debug: %s, stdout/stderr: %s",
                   any_of(log_get_options() & LogOptions::Debug) ? "yes" : "no",
                   any_of(log_get_options() & LogOptions::StdOutErr) ? "yes" : "no");

        MuLogInitialized = true;
}

void
Mu::log_uninit ()
{
        if (!MuLogInitialized)
                return;

        if (MuStream.is_open())
                MuStream.close();

        MuLogInitialized = false;
}

void
Mu::log_set_options (Mu::LogOptions opts)
{
        MuLogOptions = opts;
}

Mu::LogOptions
Mu::log_get_options ()
{
        return MuLogOptions;
}
