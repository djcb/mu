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
** along with this program; if not, write to 59the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#include "config.h"

#include <string>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>

#include <string.h>
#include <errno.h>
#include <glib/gprintf.h>
#include <gio/gio.h>

#include "glibconfig.h"
#include "mu-maildir.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"

using namespace Mu;

#define MU_MAILDIR_NOINDEX_FILE  ".noindex"
#define MU_MAILDIR_NOUPDATE_FILE ".noupdate"

/* On Linux (and some BSD), we have entry->d_type, but some file
 * systems (XFS, ReiserFS) do not support it, and set it DT_UNKNOWN.
 * On other OSs, notably Solaris, entry->d_type is not present at all.
 * For these cases, we use lstat (in get_dtype) as a slower fallback,
 * and return it in the d_type parameter
 */
static unsigned char
get_dtype(struct dirent* dentry, const std::string& path, bool use_lstat)
{
#ifdef HAVE_STRUCT_DIRENT_D_TYPE

	if (dentry->d_type == DT_UNKNOWN)
		goto slowpath;
	if (dentry->d_type == DT_LNK && !use_lstat)
		goto slowpath;

	return dentry->d_type; /* fastpath */

slowpath:
#endif /*HAVE_STRUCT_DIRENT_D_TYPE*/

	return determine_dtype(path, use_lstat);
}

static Mu::Result<void>
create_maildir(const std::string& path, mode_t mode)
{
	if (path.empty())
		return Err(Error{Error::Code::File, "path must not be empty"});

	std::array<std::string,3> subdirs = {"new", "cur", "tmp"};
	for (auto&& subdir: subdirs) {

		const auto fullpath{join_paths(path, subdir)};

		/* if subdir already exists, don't try to re-create
		 * it */
		if (check_dir(fullpath, true/*readable*/, true/*writable*/))
			continue;

		int rv{g_mkdir_with_parents(fullpath.c_str(), static_cast<int>(mode))};

		/* note, g_mkdir_with_parents won't detect an error if
		 * there's already such a dir, but with the wrong
		 * permissions; so we need to check */
		if (rv != 0 || !check_dir(fullpath, true/*readable*/, true/*writable*/))
			return Err(Error{Error::Code::File,
					"creating dir failed for {}: {}",
					fullpath, g_strerror(errno)});
	}

	return Ok();
}

static Mu::Result<void>		/* create a noindex file if requested */
create_noindex(const std::string& path)
{
	const auto noindexpath{join_paths(path, MU_MAILDIR_NOINDEX_FILE)};

	/* note, if the 'close' failed, creation may still have succeeded...*/
	int fd = ::creat(noindexpath.c_str(), 0644);
	if (fd < 0 || ::close(fd) != 0)
		return Err(Error{Error::Code::File,
				"error creating .noindex: {}", g_strerror(errno)});
	else
		return Ok();
}

Mu::Result<void>
Mu::maildir_mkdir(const std::string& path, mode_t mode, bool noindex)
{
	if (auto&& created{create_maildir(path, mode)}; !created)
		return created; // fail.
	else if (!noindex)
		return Ok();

	if (auto&& created{create_noindex(path)}; !created)
		return created; //fail

	return Ok();
}

/* determine whether the source message is in 'new' or in 'cur';
 * we ignore messages in 'tmp' for obvious reasons */
static Mu::Result<void>
check_subdir(const std::string& src, bool& in_cur)
{
	char *srcpath{g_path_get_dirname(src.c_str())};

	bool invalid{};
	if (g_str_has_suffix(srcpath, "cur"))
		in_cur = true;
	else if (g_str_has_suffix(srcpath, "new"))
		in_cur = false;
	else
		invalid = true;

	g_free(srcpath);

	if (invalid)
		return Err(Error{Error::Code::File, "invalid source message '{}'", src});
	else
		return Ok();
}

static Mu::Result<std::string>
get_target_fullpath(const std::string& src, const std::string& targetpath,
		    bool unique_names)
{
	bool in_cur{};
	if (auto&& res = check_subdir(src, in_cur); !res)
		return Err(std::move(res.error()));

	const auto srcfile{basename(src)};

	/* create target-path; note: make the filename *cough* unique by
	 * including a hash of the srcname in the targetname. This helps if
	 * there are copies of a message (which all have the same basename)
	 */
	if (unique_names)
		return join_paths(targetpath, in_cur ? "cur" : "new",
				  mu_format("{:08x}-{}", g_str_hash(src.c_str()), srcfile));
	else
		return join_paths(targetpath, in_cur ? "cur" : "new", srcfile.c_str());
}

Result<void>
Mu::maildir_link(const std::string& src, const std::string& targetpath,
		 bool unique_names)
{
	auto path_res{get_target_fullpath(src, targetpath, unique_names)};
	if (!path_res)
		return Err(std::move(path_res.error()));

	auto rv{::symlink(src.c_str(), path_res->c_str())};
	if (rv != 0)
		return Err(Error{Error::Code::File,
				"error creating link {} => {}: {}",
				*path_res, src, g_strerror(errno)});

	return Ok();
}

static bool
clear_links(const std::string& path, DIR* dir)
{
	bool res;
	struct dirent* dentry;

	res   = true;
	errno = 0;

	while ((dentry = ::readdir(dir))) {

		if (dentry->d_name[0] == '.')
			continue; /* ignore .,.. other dotdirs */

		const auto fullpath{join_paths(path, dentry->d_name)};
		const auto d_type   = get_dtype(dentry, fullpath.c_str(),
						true/*lstat*/);
		switch(d_type) {
		case DT_LNK:
			if (::unlink(fullpath.c_str()) != 0) {
				/* LCOV_EXCL_START*/
				mu_warning("error unlinking {}: {}", fullpath, g_strerror(errno));
				res = false;
				/* LCOV_EXCL_STOP*/
			} else
				mu_debug("unlinked linksdir {}", fullpath);
			break;
		case  DT_DIR: {
			DIR* subdir{::opendir(fullpath.c_str())};
			/* LCOV_EXCL_START*/
			if (!subdir) {
				mu_warning("error opening dir {}: {}", fullpath, g_strerror(errno));
				res = false;
			}
			if (!clear_links(fullpath, subdir))
				res = false;
			/* LCOV_EXCL_STOP*/
			::closedir(subdir);
		} break;
		default:
			break;
		}
	}

	return res;
}

Mu::Result<void>
Mu::maildir_clear_links(const std::string& path)
{
	const auto dir{::opendir(path.c_str())};
	if (!dir)
		return Err(Error{Error::Code::File, "failed to open {}: {}",
				path, g_strerror(errno)});

	clear_links(path, dir);
	::closedir(dir);

	return Ok();
}

/* LCOV_EXCL_START*/
static Mu::Result<void>
msg_move_verify(const std::string& src, const std::string& dst)
{
	/* double check -- is the target really there? */
	if (::access(dst.c_str(), F_OK) != 0)
		return Err(Error{Error::Code::File,
				"can't find target ({}->{})", src, dst});

	if (::access(src.c_str(), F_OK) == 0) {
		if (src ==  dst) {
			mu_warning("moved {} to itself", src);
		}
		/* this could happen if some other tool (for mail syncing) is
		 * interfering */
		mu_debug("source is still there ({}->{})", src, dst);
	}

	return Ok();
}
/* LCOV_EXCL_STOP*/

/* LCOV_EXCL_START*/
// don't use this right now, since it gives as (false alarm)
// valgrind warning in tests
/* use GIO to move files; this is slower than rename() so only use
 * this when needed: when moving across filesystems */
G_GNUC_UNUSED static Mu::Result<void>
msg_move_g_file(const std::string& src, const std::string& dst)
{
	GFile *srcfile{g_file_new_for_path(src.c_str())};
	GFile *dstfile{g_file_new_for_path(dst.c_str())};

	GError* err{};
	auto res = g_file_move(srcfile, dstfile,
			       G_FILE_COPY_OVERWRITE,
			       NULL, NULL, NULL, &err);
	g_clear_object(&srcfile);
	g_clear_object(&dstfile);

	if (res)
		return Ok();
	else
		return Err(Error::Code::File, &err, "error moving {} -> {}", src, dst);
}
/* LCOV_EXCL_STOP*/

/* use mv to move files; this is slower than rename() so only use this when
 * needed: when moving across filesystems */
G_GNUC_UNUSED static Mu::Result<void>
msg_move_mv_file(const std::string& src, const std::string& dst)
{
	if (auto res{run_command0({"/bin/mv", src, dst})}; !res)
		return Err(Error::Code::File, "error moving {}->{}; err={}", src, dst, res.error());
	else
		return Ok();
}

Mu::Result<void>
Mu::maildir_move_message(const std::string&	oldpath,
			 const std::string&	newpath,
			 bool			assume_remote)
{
	mu_debug("moving {} --> {} (assume-remote:{})", oldpath, newpath, assume_remote);

	if (::access(oldpath.c_str(), R_OK) != 0)
		return Err(Error{Error::Code::File, "cannot read {}", oldpath});

	if (oldpath == newpath)
		return Ok(); // nothing to do.

	if (!assume_remote) { /* for testing */

		if (::rename(oldpath.c_str(), newpath.c_str()) == 0) /* seems it worked; double-check */
			return msg_move_verify(oldpath, newpath);
		/* LCOV_EXCL_START*/
		if (errno != EXDEV) /* some unrecoverable error occurred */
			return Err(Error{Error::Code::File, "error moving {} -> {}: {}",
					oldpath, newpath, strerror(errno)});
		/* LCOV_EXCL_STOP*/
	}

	/* the EXDEV / assume-remote case -- source and target live on different
	 * file systems
	 *
	 * we can choose either msg_move_gio_file or msg_move_mv_file;
	 * we use the latter for now, since the former gives some (false)
	 * valgrind alarms.
	 * */
	if (auto&& res{msg_move_mv_file(oldpath, newpath)}; !res)
		return res;
	else
		return msg_move_verify(oldpath, newpath);
}

static std::string
reinvent_filename_base()
{
	return mu_format("{}.{:08x}{:08x}.{}", ::time({}),
			 g_random_int(), g_get_monotonic_time(), g_get_host_name());
}

/**
 * Determine the destination filename
 *
 * @param file a filename
 * @param flags flags for the destination
 * @param new_name whether to change the basename
 *
 * @return the destion filename.
 */
static std::string
determine_dst_filename(const std::string& file, Flags flags,
		       bool new_name)
{
	/* Recalculate a unique new base file name */
	auto&& parts{message_file_parts(file)};
	if (new_name)
		parts.base = reinvent_filename_base();

	/* for a New message, there are no flags etc.; so we only return the
	 * name sans suffix */
	if (any_of(flags & Flags::New))
		return std::move(parts.base);

	const auto flagstr{
		to_string(
			flags_filter(
				flags, MessageFlagCategory::Mailfile))};

	return parts.base + parts.separator + "2," + flagstr;
}


/*
 * sanity checks
 */
static Mu::Result<void>
check_determine_target_params (const std::string& old_path,
			       const std::string& root_maildir_path,
			       const std::string& target_maildir,
			       Flags newflags)
{
	if (!g_path_is_absolute(old_path.c_str()))
		return Err(Error{Error::Code::File,
				"old_path is not absolute ({})", old_path});

	if (!g_path_is_absolute(root_maildir_path.c_str()))
		return Err(Error{Error::Code::File,
				"root maildir path is not absolute ({})",
				root_maildir_path});

	if (!target_maildir.empty() && target_maildir[0] != '/')
		return Err(Error{Error::Code::File,
				"target maildir must be empty or start with / ({})",
				target_maildir});

	if (old_path.find(root_maildir_path) != 0)
		return Err(Error{Error::Code::File,
				"old-path must be below root-maildir ({}) ({})",
				old_path, root_maildir_path});

	if (any_of(newflags & Flags::New) && newflags != Flags::New)
		return Err(Error{Error::Code::File,
					"if the New flag is specified, it must be the only flag"});
	return Ok();
}


Mu::Result<std::string>
Mu::maildir_determine_target(const std::string&	old_path,
			     const std::string& root_maildir_path,
			     const std::string&	target_maildir,
			     Flags		newflags,
			     bool		new_name)
{
	newflags = flags_maildir_file(newflags); // filter out irrelevant flags.

	/* sanity checks */
	if (const auto checked{check_determine_target_params(
		old_path, root_maildir_path, target_maildir, newflags)}; !checked)
		return Err(Error{std::move(checked.error())});
	/*
	 * this gets us the source maildir filesystem path, the directory
	 * in which new/ & cur/ lives, and the source file
	 */
	const auto src{base_message_dir_file(old_path)};
	if (!src)
		return Err(src.error());
	const auto& [src_mdir, src_file, is_new] = *src;

	/* if target_mdir is empty, the src_dir does not change (though cur/
	 * maybe become new or vice-versa) */
	const auto dst_mdir = target_maildir.empty() ? src_mdir :
		join_paths(root_maildir_path,  target_maildir);

	/* now calculate the message name (incl. its immediate parent dir) */
	const auto dst_file{determine_dst_filename(src_file, newflags, new_name)};

	/* and the complete path name. */
	const std::string subdir{(none_of(newflags & Flags::New)) ? "cur" : "new"};

	return join_paths(dst_mdir, subdir,dst_file);
}
