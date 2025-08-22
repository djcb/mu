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
to_store(SCM scm, const char *func, int pos)
{
	if (!SCM_IS_A_P(scm, store_type))
		throw ScmError{ScmError::Id::WrongType, func, pos, scm, "store"};

	return *reinterpret_cast<Store*>(scm_foreign_object_ref(scm, 0));
}

static SCM
subr_cc_store_alist(SCM store_scm) try {
	constexpr auto func{"cc-store-alist"};

	SCM alist{SCM_EOL};
	const auto& conf{to_store(store_scm, func, 1).config()};

	using MuConfig = Mu::Config;
	using Type = MuConfig::Type;

	for (const auto& prop: Mu::Config::properties) {

		// don't expose internal values & values that may change during
		// runtime
		if (any_of(prop.flags &
			   (MuConfig::Flags::Internal | MuConfig::Flags::Runtime)))
			continue;

		const auto str{conf.get_str(prop)};
		if (str.empty())
			continue;

		const auto name{make_symbol(prop.name)};
		const auto val = std::invoke([&]() {
			switch (prop.type) {
			case Type::Number:
				return to_scm(MuConfig::decode<Type::Number>(str));
			case Type::Boolean:
				return to_scm(MuConfig::decode<Type::Boolean>(str));
			case Type::Timestamp:
				return to_scm(MuConfig::decode<Type::Timestamp>(str));
			case Type::Path:
				return to_scm(MuConfig::decode<Type::Path>(str));
			case Type::String:
				return to_scm(MuConfig::decode<Type::String>(str));
			case Type::StringList:
				return to_scm(MuConfig::decode<Type::StringList>(str));
			default:
				throw ScmError{ScmError::Id::WrongType, func, 1, store_scm, "store"};
			}
		});

		alist = scm_acons(name, val, alist);
	}

	return scm_reverse_x(alist, SCM_EOL);

} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_store_mcount(SCM store_scm) try {
	return to_scm(to_store(store_scm, "cc-store-mcount", 1).size());
} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_store_cfind(SCM store_scm, SCM pattern_scm, SCM personal_scm, SCM after_scm, SCM max_results_scm) try {

	constexpr auto func{"cc-store-cfind"};

	SCM contacts{SCM_EOL};
	const auto pattern{from_scm<std::string>(pattern_scm, func, 2)};
	const auto personal{from_scm<bool>(personal_scm, func, 3)};
	const auto after{from_scm_with_default(after_scm, 0, func, 4)};
	// 0 means "unlimited"
	const size_t maxnum = from_scm_with_default(max_results_scm, 0U, func, 5);

	to_store(store_scm, func, 1).contacts_cache().for_each(
		[&](const auto& contact)->bool {
			contacts = scm_cons(to_scm(contact), contacts);
			return true;
		}, pattern, personal, after, maxnum);

	return scm_reverse_x(contacts, SCM_EOL);
} catch (const ScmError& scm_err) {
	scm_err.throw_scm();
}

static Field::Id
to_sort_field_id(SCM field, const char *func, int pos)
{
	if (scm_is_false(field))
		return Field::Id::Date;

	const auto sym{from_scm<std::string>(scm_symbol_to_string(field), func, pos)};
	if (const auto field_opt{field_from_name(sym)}; !field_opt) {
		throw ScmError{ScmError::Id::WrongType, func, pos, field, "sort-field-symbol"};
	} else
		return field_opt->id;
}

static SCM
subr_cc_store_mfind(SCM store_scm, SCM query_scm, SCM related_scm, SCM skip_dups_scm,
	  SCM sort_field_scm, SCM reverse_scm, SCM max_results_scm) try {

	constexpr auto func{"cc-store-mfind"};

	const auto& store{to_store(store_scm, func, 1)};
	const auto query{from_scm<std::string>(query_scm, func, 2)};
	const auto related(from_scm<bool>(related_scm, func, 3));
	const auto skip_dups(from_scm<bool>(skip_dups_scm, func, 4));

	if (!scm_is_false(sort_field_scm) && !scm_is_symbol(sort_field_scm))
		throw ScmError{ScmError::Id::WrongType, func, 5, sort_field_scm, "#f or sort-field-symbol"};

	const auto sort_field_id = to_sort_field_id(sort_field_scm, func, 5);
	const auto reverse(from_scm<bool>(reverse_scm, func, 6));

	// 0 means "unlimited"
	const size_t maxnum = from_scm_with_default(max_results_scm, 0U, func, 7);

	const QueryFlags qflags = QueryFlags::SkipUnreadable |
		(skip_dups ? QueryFlags::SkipDuplicates : QueryFlags::None) |
		(related ? QueryFlags::IncludeRelated: QueryFlags::None ) |
		(reverse ? QueryFlags::Descending : QueryFlags::None);

	std::lock_guard lock{store.lock()};
	const auto qres{store.run_query(query, sort_field_id, qflags, maxnum)};
	if (!qres)
		throw ScmError{ScmError::Id::WrongArg, func, 2, query_scm, ""};

	SCM msgs{SCM_EOL};
	// iterate in reverse order, so the message get consed
	// into the list in the right order.
	for (auto it{qres->end()}; it-- != qres->begin();)
		if (auto plist{it.document()->get_data()}; !plist.empty())
			msgs = scm_cons(to_scm(plist), msgs);
	return msgs;

} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_store_all_labels(SCM store_scm) try {

	constexpr auto func{"cc-store-all-labels"};
	const auto& store{to_store(store_scm, func, 1)};

	const auto label_map{store.label_map()};

	SCM labels{SCM_EOL};
	for (const auto& [label, _n]: label_map)
		labels = scm_append_x(
			scm_list_2(labels,
				   scm_list_1(to_scm<std::string>(label))));
	return labels;

} catch (const ScmError& err) {
	err.throw_scm();
}


static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("cc-store-mfind", 7/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_store_mfind));
	scm_c_define_gsubr("cc-store-mcount", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_store_mcount));
	scm_c_define_gsubr("cc-store-cfind", 5/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_store_cfind));
	scm_c_define_gsubr("cc-store-alist", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_store_alist));
	scm_c_define_gsubr("cc-store-all-labels", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_store_all_labels));
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
	scm_c_define("%cc-default-store", default_store);

	init_subrs();

	initialized = true;
}


SCM
Mu::Scm::to_scm(const Contact& contact)
{
	static SCM email{make_symbol("email")};
	static SCM name{make_symbol("name")};

	SCM alist = scm_acons(email, to_scm(contact.email), SCM_EOL);
	if (!contact.name.empty())
		alist = scm_acons(name, to_scm(contact.name), alist);

	return alist;
}
