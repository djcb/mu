/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H */

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.h"
#include "src/mu-msg-file.h"

static void test_mu_msg_file_get_flags_from_path(void)
{
	int i;

	struct {
		const char *path;
		MuMsgFlags flags;
	} paths[] = {
		{
		"/home/foo/Maildir/test/cur/123456:2,FSR",
			    MU_MSG_FLAG_REPLIED | MU_MSG_FLAG_SEEN | MU_MSG_FLAG_FLAGGED}, {
		"/home/foo/Maildir/test/new/123456",
		            MU_MSG_FLAG_NEW | MU_MSG_FLAG_UNREAD}, {
		"/home/foo/Maildir/test/new/123456:2,FR",
			    MU_MSG_FLAG_NEW | MU_MSG_FLAG_UNREAD}, {
		"/home/foo/Maildir/test/cur/123456:2,DTP",
			    MU_MSG_FLAG_DRAFT | MU_MSG_FLAG_TRASHED |
			    MU_MSG_FLAG_PASSED | MU_MSG_FLAG_UNREAD }, {
		"/home/foo/Maildir/test/cur/123456:2,S",
			    MU_MSG_FLAG_SEEN}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		MuMsgFlags flags;
		flags = mu_msg_file_get_flags_from_path(paths[i].path);
		g_assert_cmpuint(flags, ==, paths[i].flags);
	}
}

static void test_mu_msg_file_get_path_from_flags(void)
{
	int i;

	struct {
		const char *oldpath;
		MuMsgFlags flags;
		const char *newpath;
	} paths[] = {
		{
		"/home/foo/Maildir/test/cur/123456:2,FR",
			    MU_MSG_FLAG_REPLIED,
			    "/home/foo/Maildir/test/cur/123456:2,R"}, {
		"/home/foo/Maildir/test/cur/123456:2,FR",
			    MU_MSG_FLAG_NEW,
			    "/home/foo/Maildir/test/new/123456"}, {
		"/home/foo/Maildir/test/new/123456:2,FR",
			    MU_MSG_FLAG_SEEN | MU_MSG_FLAG_REPLIED,
			    "/home/foo/Maildir/test/cur/123456:2,RS"}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		gchar *str;
		str = mu_msg_file_get_path_from_flags(paths[i].oldpath,
						      paths[i].flags);
		g_assert_cmpstr(str, ==, paths[i].newpath);
		g_free(str);
	}
}

int main(int argc, char *argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/mu-msg-file/mu-msg-file-get-path-from-flags",
			test_mu_msg_file_get_path_from_flags);
	g_test_add_func("/mu-msg-file/mu-msg-file-get-flags-from-path",
			test_mu_msg_file_get_flags_from_path);

	g_log_set_handler(NULL,
			  G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL |
			  G_LOG_FLAG_RECURSION, (GLogFunc) black_hole, NULL);

	return g_test_run();
}
