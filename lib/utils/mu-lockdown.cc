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

#include "mu-lockdown.hh"

#include <sys/mman.h>
#include <sys/resource.h>
#include <unistd.h>

#include <algorithm>
#include <cerrno>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <glib.h>

using namespace Mu;

constexpr size_t max_mem = 256 << 20; /*MiB*/

/**
 * Handler for std::terminate. Report and exit, abort() is prohibited.
 */
[[noreturn]] static void
terminate_handler()
{
	const char *what{"unknown error"};
	try {
		if (auto ex{std::current_exception()}; ex)
			std::rethrow_exception(ex);
	} catch (const std::exception& ex) {
		what = ex.what();
	} catch (...) {}

	::fputs("error: ", stderr);
	::fputs(what, stderr);
	::fputs(" (terminated)\n", stderr);

	std::_Exit(1);
}


// Linux-specific lock-down; uses close_range, prctl and seccomp
#if defined(__linux__) && HAVE_LOCKDOWN_HELPER
#include <sys/prctl.h>
#include <seccomp.h>

static int
lockdown_linux(int fd)
{
	// close all file-descriptors, except for fd and stderr
	const auto [lo, hi] = std::minmax({fd, STDERR_FILENO});
	if (lo > 0 && close_range(0, lo - 1, 0) != 0)
		return -1;
	if (hi > lo + 1 && close_range(lo + 1, hi - 1, 0) != 0)
		return -1;
	if (close_range(hi + 1, ~0U, 0) != 0)
		return -1;

	if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0)
		return -1;

	// avoid core-dumps
	if (prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) != 0)
		return -1;

	scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_KILL_PROCESS);
	if (!ctx)
		return -1;

	int rv{};
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(read),  1,
			       SCMP_A0(SCMP_CMP_EQ, static_cast<scmp_datum_t>(fd)));
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(write), 1,
			       SCMP_A0(SCMP_CMP_EQ, static_cast<scmp_datum_t>(fd)));
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(write), 1,
			       SCMP_A0(SCMP_CMP_EQ, static_cast<scmp_datum_t>(STDERR_FILENO)));
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(brk), 0);
	/* allow mmap/mprotect, but never executable memory */
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(mmap), 1,
			       SCMP_A2(SCMP_CMP_MASKED_EQ, PROT_EXEC, 0));
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(mprotect), 1,
			       SCMP_A2(SCMP_CMP_MASKED_EQ, PROT_EXEC, 0));
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(munmap), 0);
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(mremap), 0);
	/* for locks, once-initialization etc. */
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(futex), 0);
	rv |= seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(exit_group), 0);

	/* accessing the file-system is not allowed, but libraries may try
	 * (e.g. poppler looking for optional data-files), so fail with ENOENT
	 * rather than kill. Not all of these exist on all architectures;
	 * libseccomp handles that. */
	for (const auto syscall: {
			SCMP_SYS(open), SCMP_SYS(openat), SCMP_SYS(openat2),
			SCMP_SYS(stat), SCMP_SYS(lstat), SCMP_SYS(newfstatat),
			SCMP_SYS(statx),
			SCMP_SYS(access), SCMP_SYS(faccessat), SCMP_SYS(faccessat2),
			SCMP_SYS(readlink), SCMP_SYS(readlinkat)})
		rv |= seccomp_rule_add(ctx, SCMP_ACT_ERRNO(ENOENT), syscall, 0);
	if (rv == 0)
		rv = seccomp_load(ctx);

	seccomp_release(ctx);

	return rv == 0 ? 0 : -1;
}
#endif // __linux__ && HAVE_LOCKDOWN_HELPER

static void
lockdown_common(int fd, size_t timeout_s) noexcept
{
	std::set_terminate(terminate_handler);

	// limit maximum mem
	struct rlimit mem = { max_mem, max_mem };
	if (setrlimit(RLIMIT_AS, &mem) != 0)
		g_error("cannot set memory limit; terminating");

	if (timeout_s > 0) { // limit maximum time, if requested
		struct rlimit cpu = { timeout_s, timeout_s };
		if (setrlimit(RLIMIT_CPU, &cpu) != 0)
			g_error("cannot set time limit; terminating");
		// SIGALRM to terminate a too-slow/hanging process
		sigset_t sigalrm;
		::sigemptyset(&sigalrm);
		::sigaddset(&sigalrm, SIGALRM);
		if (::signal(SIGALRM, SIG_DFL) == SIG_ERR ||
		    ::sigprocmask(SIG_UNBLOCK, &sigalrm, {}) != 0)
			g_error("cannot set time limit; terminating");
		::alarm(static_cast<unsigned>(3 * timeout_s));
	}

	// avoid core-dumps
	struct rlimit core = { 0, 0 };
	if (setrlimit(RLIMIT_CORE, &core) != 0)
		g_error("cannot disable core-dumps; terminating");

	// while we still can...
	tzset();
	if (fd == STDOUT_FILENO)
		::setvbuf(stdout, {}, _IOFBF, BUFSIZ);
}

void
Mu::lockdown_process(int fd, size_t timeout_s)
{
	lockdown_common(fd, timeout_s);

#if defined(__linux__) && HAVE_LOCKDOWN_HELPER
	if (lockdown_linux(fd) == 0) {
		// glib's default log-writer is not allowed after lock-down
		g_log_set_writer_func([](auto...) { return G_LOG_WRITER_HANDLED; },
				      {}, {});
		return;
	}
	// #elif defined (....)
	//
	//
#endif
	g_error("cannot lock-down process; terminating");
	::exit(1); // not reached; g_error aborts
}
