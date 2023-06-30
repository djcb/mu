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


#include "mu-xapian-db.hh"
#include "utils/mu-utils.hh"
#include <inttypes.h>

#include <mutex>

using namespace Mu;

struct XapianDb::Private {
	using DbType = std::variant<Xapian::Database, Xapian::WritableDatabase>;

	Private(Xapian::Database&& db, const std::string& path):
		db_{std::move(db)}, path_{path} {}
	Private(Xapian::WritableDatabase&& wdb, const std::string& path):
		db_{std::move(wdb)}, path_{path} {}

	DbType			db_;
	const std::string	path_;
	mutable std::mutex	lock_;
};


XapianDb::XapianDb(Xapian::Database&& db, const std::string& path):
	priv_{std::make_unique<Private>(std::move(db), path)}
{}
XapianDb::XapianDb(Xapian::WritableDatabase&& wdb, const std::string& path):
	priv_{std::make_unique<Private>(std::move(wdb), path)}
{}

XapianDb::XapianDb(XapianDb&& rhs) = default;
XapianDb::~XapianDb() = default;


const Xapian::Database&
XapianDb::db() const
{
	if (std::holds_alternative<Xapian::WritableDatabase>(priv_->db_))
		return std::get<Xapian::WritableDatabase>(priv_->db_);
	else
		return std::get<Xapian::Database>(priv_->db_);
}

Xapian::WritableDatabase&
XapianDb::wdb()
{
	if (read_only())
		throw std::runtime_error("database is read-only");
	return std::get<Xapian::WritableDatabase>(priv_->db_);
}

bool
XapianDb::read_only() const
{
	return !std::holds_alternative<Xapian::WritableDatabase>(priv_->db_);
}

const std::string&
XapianDb::path() const
{
	return priv_->path_;
}

std::mutex&
XapianDb::lock() const
{
	return priv_->lock_;
}

void
XapianDb::set_timestamp(const std::string_view key)
{
	wdb().set_metadata(std::string{key},
			   format("%" PRIi64, static_cast<int64_t>(::time({}))));
}

Result<XapianDb>
XapianDb::make(const std::string& db_path, Flavor flavor) noexcept try {

	if (flavor != Flavor::ReadOnly) {
		/* we do our own flushing, set Xapian's internal one as
		 * the backstop*/
		g_setenv("XAPIAN_FLUSH_THRESHOLD", "500000", 1);
		/* create path if needed */
		if (g_mkdir_with_parents(db_path.c_str(), 0700) != 0)
			return Err(Error::Code::File, "failed to create database dir %s: %s",
				   db_path.c_str(), ::strerror(errno));
	}

	switch (flavor) {

	case Flavor::ReadOnly:
		return Ok(XapianDb(Xapian::Database(db_path), db_path));
	case Flavor::Open:
		return Ok(XapianDb(Xapian::WritableDatabase(
					   db_path, Xapian::DB_OPEN), db_path));
	case Flavor::CreateOverwrite: {
		auto&& xdb{XapianDb(Xapian::WritableDatabase(
					    db_path,
					    Xapian::DB_CREATE_OR_OVERWRITE), db_path)};
		xdb.set_timestamp(MetadataIface::created_key);
		return Ok(std::move(xdb));
	}
	default:
		return Err(Error::Code::Internal, "invalid xapian options");
	}

} catch (const Xapian::DatabaseLockError& xde) {
	return Err(Error::Code::StoreLock, "%s", xde.get_msg().c_str());
} catch (const Xapian::DatabaseError& xde) {
	return Err(Error::Code::Store, "%s", xde.get_msg().c_str());
} catch (const Mu::Error& me) {
	return Err(me);
} catch (...) {
	return Err(Error::Code::Internal,
		   "something went wrong when opening store @ %s", db_path.c_str());
}
