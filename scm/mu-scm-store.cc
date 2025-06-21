/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-scm-types.hh"

using namespace Mu;
using namespace Mu::Scm;

// types
namespace {
static SCM store_type;
static SCM default_store;
static bool initialized;
}

static const Store&
to_store(SCM scm)
{
	scm_assert_foreign_object_type(store_type, scm);
	return *reinterpret_cast<Store*>(scm_foreign_object_ref(scm, 0));
}

static SCM
subr_mcount(SCM store_scm)
{
	return to_scm(to_store(store_scm).size());
}

static SCM
subr_cfind(SCM store_scm, SCM pattern_scm, SCM personal_scm, SCM after_scm, SCM max_results_scm)
{
	SCM contacts{SCM_EOL};
	const auto pattern{from_scm<std::string>(pattern_scm)};
	const auto personal{from_scm<bool>(personal_scm)};
	const auto after{from_scm_with_default(after_scm, 0)};

	// 0 means "unlimited"
	const size_t maxnum = from_scm_with_default(max_results_scm, 0U);

	to_store(store_scm).contacts_cache().for_each(
		[&](const auto& contact)->bool {
			contacts = scm_append_x(scm_list_2(contacts, scm_list_1(to_scm(contact))));
			return true;
		}, pattern, personal, after, maxnum);
	return contacts;
}

static Field::Id
to_sort_field_id(SCM field)
{
	if (scm_is_false(field))
		return Field::Id::Date;

	const auto sym{from_scm<std::string>(scm_symbol_to_string(field))};
	if (const auto field_opt{field_from_name(sym)}; !field_opt) {
		raise_error("invalid symbol", "mfind",
			    "expected sort-field symbol, but got {}", sym);
		return Field::Id::Date;
	} else
		return field_opt->id;
}

static SCM
subr_mfind(SCM store_scm, SCM query_scm, SCM related_scm, SCM skip_dups_scm,
	  SCM sort_field_scm, SCM reverse_scm, SCM max_results_scm)
{
	const auto& store{to_store(store_scm)};
	const auto query{from_scm<std::string>(query_scm)};
	const auto related(from_scm<bool>(related_scm));
	const auto skip_dups(from_scm<bool>(skip_dups_scm));
	const auto reverse(from_scm<bool>(reverse_scm));

	SCM_ASSERT_TYPE(scm_is_false(sort_field_scm) || scm_is_symbol(sort_field_scm),
			sort_field_scm, SCM_ARG5, __func__, "symbol or #f");

	const auto sort_field_id = to_sort_field_id(sort_field_scm);

	// 0 means "unlimited"
	const size_t maxnum = from_scm_with_default(max_results_scm, 0U);

	// XXX date/reverse/maxnum

	const QueryFlags qflags = QueryFlags::SkipUnreadable |
		(skip_dups ? QueryFlags::SkipDuplicates : QueryFlags::None) |
		(related ? QueryFlags::IncludeRelated: QueryFlags::None ) |
		(reverse ? QueryFlags::Descending : QueryFlags::None);

	SCM msgs{SCM_EOL};
	std::lock_guard lock{store.lock()};
	const auto qres = store.run_query(query, sort_field_id, qflags, maxnum);

	SCM_ASSERT(qres, query_scm, SCM_ARG1, __func__);

	for (const auto& mi: *qres) {
		if (auto plist{mi.document()->get_data()}; plist.empty())
			continue;
		else {
			SCM scm_plist{scm_c_eval_string(("'" + plist).c_str())};
			msgs = scm_append_x(scm_list_2( msgs, scm_list_1(scm_plist)));
		}
	}

	return msgs;
}

static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("store-mfind", 7/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_mfind));
	scm_c_define_gsubr("store-mcount", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_mcount));
	scm_c_define_gsubr("store-cfind", 5/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cfind));
#pragma GCC diagnostic pop
}


void
Mu::Scm::init_store(const Store& store)
{
	if (initialized)
		return;

	store_type = scm_make_foreign_object_type(
		make_symbol("store"),
		scm_list_1(make_symbol("data")),
		{});// no finalizer

	default_store = scm_make_foreign_object_1(
		store_type, const_cast<Store*>(&store));
	scm_c_define("%default-store-object", default_store);

	init_subrs();

	initialized = true;
}


SCM
Mu::Scm::to_scm(const Contact& contact)
{
	static SCM email{scm_from_utf8_symbol("email")};
	static SCM name{scm_from_utf8_symbol("name")};

	SCM alist = scm_acons(email, to_scm(contact.email), SCM_EOL);
	if (!contact.name.empty())
		alist = scm_acons(name, to_scm(contact.name), alist);

	return alist;
}
