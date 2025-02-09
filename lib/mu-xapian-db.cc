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
#include <mu-config.hh>

#include <mutex>

using namespace Mu;

const Xapian::Database&
XapianDb::db() const
{
	if (std::holds_alternative<Xapian::WritableDatabase>(db_))
		return std::get<Xapian::WritableDatabase>(db_);
	else
		return std::get<Xapian::Database>(db_);
}

Xapian::WritableDatabase&
XapianDb::wdb()
{
	if (read_only())
		throw std::runtime_error("database is read-only");

	return std::get<Xapian::WritableDatabase>(db_);
}

bool
XapianDb::read_only() const
{
	return !std::holds_alternative<Xapian::WritableDatabase>(db_);
}

void
XapianDb::set_timestamp(const std::string_view key)
{
	wdb().set_metadata(std::string{key}, mu_format("{}", ::time({})));
}

using Flavor = XapianDb::Flavor;

static std::string
make_path(const std::string& db_path, Flavor flavor)
{
	if (flavor != Flavor::ReadOnly) {
		/* we do our own flushing, set Xapian's internal one as
		 * the backstop*/
		g_setenv("XAPIAN_FLUSH_THRESHOLD", "500000", 1);
		/* create path if needed */
		if (g_mkdir_with_parents(db_path.c_str(), 0700) != 0)
			throw Error(Error::Code::File, "failed to create database dir {}: {}",
				    db_path, ::strerror(errno));
	}

	return db_path;
}

static XapianDb::DbType
make_db(const std::string& db_path, Flavor flavor)
{
	switch (flavor) {

	case Flavor::ReadOnly:
		return Xapian::Database(db_path);
	case Flavor::Open:
		return Xapian::WritableDatabase(db_path, Xapian::DB_OPEN);
	case Flavor::CreateOverwrite:
		return Xapian::WritableDatabase(db_path, Xapian::DB_CREATE_OR_OVERWRITE);
		/* LCOV_EXCL_START*/
	default:
		throw std::logic_error("unknown flavor");
		/* LCOV_EXCL_STOP*/
	}
}

XapianDb::XapianDb(const std::string& db_path, Flavor flavor):
	path_(make_path(db_path, flavor)),
	db_(make_db(path_, flavor)),
	batch_size_{Config(*this).get<Config::Id::BatchSize>()/*default*/} {
	if (flavor == Flavor::CreateOverwrite)
		set_timestamp(MetadataIface::created_key);

	mu_debug("created {}", *this);
}

void
XapianDb::reinit() {
	batch_size_ = Config(*this).get<Config::Id::BatchSize>();
	mu_debug("set batch-size to {}", batch_size_);
}


#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"
#include "config.h"
#include "mu-store.hh"

static void
test_errors()
{
	allow_warnings();

	TempDir tdir;
	auto store = Store::make_new(tdir.path(), MU_TESTMAILDIR2);
	assert_valid_result(store);
	g_assert_true(store->empty());

	XapianDb xdb(tdir.path(), Flavor::ReadOnly);
	g_assert_true(xdb.read_only());

	g_assert_false(!!xdb.delete_document("Boo"));
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/xapian-db/errors", test_errors);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
