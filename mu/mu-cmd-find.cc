 /*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <array>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>

#include "message/mu-message.hh"
#include "mu-maildir.hh"
#include "mu-query-match-deciders.hh"
#include "mu-query.hh"
#include "mu-bookmarks.hh"
#include "message/mu-message.hh"

#include "utils/mu-option.hh"

#include "mu-cmd.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

using Format = Options::Find::Format;

struct OutputInfo {
	Xapian::docid       docid{};
	bool                header{};
	bool                footer{};
	bool                last{};
	Option<QueryMatch&> match_info;
};

constexpr auto FirstOutput{OutputInfo{0, true, false, {}, {}}};
constexpr auto LastOutput{OutputInfo{0, false, true, {}, {}}};

using OutputFunc = std::function<bool(const Option<Message>& msg, const OutputInfo&,
				      const Options&, GError**)>;

using Format = Options::Find::Format;

static Result<void>
print_internal(const Store&       store,
	       const std::string& expr,
	       bool           xapian,
	       bool           warn)
{
	std::cout << store.parse_query(expr, xapian) << "\n";
	return Ok();
}

static Result<QueryResults>
run_query(const Store& store, const std::string& expr, const Options& opts)
{
	Mu::QueryFlags qflags{QueryFlags::SkipUnreadable};
	if (opts.find.reverse)
		qflags |= QueryFlags::Descending;
	if (opts.find.skip_dups)
		qflags |= QueryFlags::SkipDuplicates;
	if (opts.find.include_related)
		qflags |= QueryFlags::IncludeRelated;
	if (opts.find.threads)
		qflags |= QueryFlags::Threading;

	return store.run_query(expr,
			       opts.find.sortfield,
			       qflags, opts.find.maxnum.value_or(0));
}

static bool
exec_cmd(const Option<Message>& msg, const OutputInfo& info, const Options& opts, GError** err)
{
	if (!msg)
		return true;

	gint     status;
	char *   cmdline, *escpath;
	bool rv;

	escpath = g_shell_quote(msg->path().c_str());
	cmdline = g_strdup_printf("%s %s", opts.find.exec.c_str(), escpath);

	rv = g_spawn_command_line_sync(cmdline, NULL, NULL, &status, err);

	g_free(cmdline);
	g_free(escpath);

	return rv;
}

static Result<std::string>
resolve_bookmark(const Options& opts)
{
	const auto bmfile = opts.runtime_path(RuntimePath::Bookmarks);
	auto bm     = mu_bookmarks_new(bmfile.c_str());
	if (!bm)
		return Err(Error::Code::File,
			   "failed to open bookmarks file '%s'", bmfile.c_str());

	const auto bookmark{opts.find.bookmark};
	const auto val = mu_bookmarks_lookup(bm, bookmark.c_str());
	if (!val) {
		mu_bookmarks_destroy(bm);
		return Err(Error::Code::NoMatches,
			   "bookmark '%s' not found", bookmark.c_str());
	}

	mu_bookmarks_destroy(bm);
	return Ok(std::string(val));
}

static Result<std::string>
get_query(const Options& opts)
{
	if (opts.find.bookmark.empty() && opts.find.query.empty())
		return Err(Error::Code::InvalidArgument,
			   "neither bookmark nor query");

	std::string bookmark;
	if (!opts.find.bookmark.empty()) {
		const auto res = resolve_bookmark(opts);
		if (!res)
			return Err(std::move(res.error()));
		bookmark = res.value() + " ";
	}

	auto&& query{join(opts.find.query, " ")};
	return Ok(bookmark + query);
}

static bool
prepare_links(const Options& opts, GError** err)
{
	/* note, mu_maildir_mkdir simply ignores whatever part of the
	 * mail dir already exists */
	if (auto&& res = maildir_mkdir(opts.find.linksdir, 0700, true); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	if (!opts.find.clearlinks)
		return false;

	if (auto&& res = maildir_clear_links(opts.find.linksdir); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	return true;
}

static bool
output_link(const Option<Message>& msg, const OutputInfo& info, const Options& opts, GError** err)
{
	if (info.header)
		return prepare_links(opts, err);
	else if (info.footer)
		return true;

	/* during test, do not create "unique names" (i.e., names with path
	 * hashes), so we get a predictable result */
	const auto unique_names{!g_getenv("MU_TEST")&&!g_test_initialized()};

	if (auto&& res = maildir_link(msg->path(), opts.find.linksdir, unique_names); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	return true;
}

static void
ansi_color_maybe(Field::Id field_id, bool color)
{
	const char* ansi;

	if (!color)
		return; /* nothing to do */

	switch (field_id) {
	case Field::Id::From: ansi = MU_COLOR_CYAN; break;

	case Field::Id::To:
	case Field::Id::Cc:
	case Field::Id::Bcc: ansi     = MU_COLOR_BLUE; break;
	case Field::Id::Subject: ansi = MU_COLOR_GREEN; break;
	case Field::Id::Date: ansi    = MU_COLOR_MAGENTA; break;

	default:
		if (field_from_id(field_id).type != Field::Type::String)
			ansi = MU_COLOR_YELLOW;
		else
			ansi = MU_COLOR_RED;
	}

	fputs(ansi, stdout);
}

static void
ansi_reset_maybe(Field::Id field_id, bool color)
{
	if (!color)
		return; /* nothing to do */

	fputs(MU_COLOR_DEFAULT, stdout);
}

static std::string
display_field(const Message& msg, Field::Id field_id)
{
	switch (field_from_id(field_id).type) {
	case Field::Type::String:
		return msg.document().string_value(field_id);
	case Field::Type::Integer:
		if (field_id == Field::Id::Priority) {
			return to_string(msg.priority());
		} else if (field_id == Field::Id::Flags) {
			return to_string(msg.flags());
		} else /* as string */
			return msg.document().string_value(field_id);
	case Field::Type::TimeT:
		return time_to_string(
			"%c", static_cast<::time_t>(msg.document().integer_value(field_id)));
	case Field::Type::ByteSize:
		return to_string(msg.document().integer_value(field_id));
	case Field::Type::StringList:
		return join(msg.document().string_vec_value(field_id), ',');
	case Field::Type::ContactList:
		return to_string(msg.document().contacts_value(field_id));
	default:
		g_return_val_if_reached("");
		return "";
	}
}

static void
print_summary(const Message& msg, const Options& opts)
{
	const auto body{msg.body_text()};
	if (!body)
		return;

	const auto summ{summarize(body->c_str(), opts.find.summary_len.value_or(0))};

	g_print("Summary: ");
	fputs_encoded(summ, stdout);
	g_print("\n");
}

static void
thread_indent(const QueryMatch& info, const Options& opts)
{
	const auto is_root{any_of(info.flags & QueryMatch::Flags::Root)};
	const auto first_child{any_of(info.flags & QueryMatch::Flags::First)};
	const auto last_child{any_of(info.flags & QueryMatch::Flags::Last)};
	const auto empty_parent{any_of(info.flags & QueryMatch::Flags::Orphan)};
	const auto is_dup{any_of(info.flags & QueryMatch::Flags::Duplicate)};
	// const auto is_related{any_of(info.flags & QueryMatch::Flags::Related)};

	/* indent */
	if (opts.debug) {
		::fputs(info.thread_path.c_str(), stdout);
		::fputs(" ", stdout);
	} else
		for (auto i = info.thread_level; i > 1; --i)
			::fputs("  ", stdout);

	if (!is_root) {
		if (first_child)
			::fputs("\\", stdout);
		else if (last_child)
			::fputs("/", stdout);
		else
			::fputs(" ", stdout);
		::fputs(empty_parent ? "*> " : is_dup ? "=> "
						      : "-> ",
			stdout);
	}
}

static void
output_plain_fields(const Message& msg, const std::string& fields,
		    bool color, bool threads)
{
	size_t nonempty{};

	for (auto&& k: fields) {
		const auto field_opt{field_from_shortcut(k)};
		if (!field_opt || (!field_opt->is_value() && !field_opt->is_contact()))
			nonempty += printf("%c", k);

		else {
			ansi_color_maybe(field_opt->id, color);
			nonempty += fputs_encoded(
				display_field(msg, field_opt->id), stdout);
			ansi_reset_maybe(field_opt->id, color);
		}
	}

	if (nonempty)
		fputs("\n", stdout);
}

static bool
output_plain(const Option<Message>& msg, const OutputInfo& info,
	     const Options& opts, GError** err)
{
	if (!msg)
		return true;

	/* we reuse the color (whatever that may be)
	 * for message-priority for threads, too */
	ansi_color_maybe(Field::Id::Priority, !opts.nocolor);
	if (opts.find.threads && info.match_info)
		thread_indent(*info.match_info, opts);

	output_plain_fields(*msg, opts.find.fields, !opts.nocolor, opts.find.threads);

	if (opts.view.summary_len)
		print_summary(*msg, opts);

	return true;
}

static bool
output_sexp(const Option<Message>& msg, const OutputInfo& info, const Options& opts, GError** err)
{
	if (msg) {
		if (const auto sexp{msg->sexp()}; !sexp.empty())
			fputs(sexp.to_string().c_str(), stdout);
		else
			fputs(msg->sexp().to_string().c_str(), stdout);
		fputs("\n", stdout);
	}

	return true;
}

static bool
output_json(const Option<Message>& msg, const OutputInfo& info, const Options& opts, GError** err)
{
	if (info.header) {
		g_print("[\n");
		return true;
	}

	if (info.footer) {
		g_print("]\n");
		return true;
	}

	if (!msg)
		return true;

	g_print("%s%s\n",
		msg->sexp().to_json_string().c_str(),
		info.last ? "" : ",");

	return true;
}

static void
print_attr_xml(const std::string& elm, const std::string& str)
{
	if (str.empty())
		return; /* empty: don't include */

	auto&& esc{to_string_opt_gchar(g_markup_escape_text(str.c_str(), -1))};
	g_print("\t\t<%s>%s</%s>\n", elm.c_str(), esc.value_or("").c_str(), elm.c_str());
}

static bool
output_xml(const Option<Message>& msg, const OutputInfo& info, const Options& opts, GError** err)
{
	if (info.header) {
		g_print("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
		g_print("<messages>\n");
		return true;
	}

	if (info.footer) {
		g_print("</messages>\n");
		return true;
	}

	g_print("\t<message>\n");
	print_attr_xml("from", to_string(msg->from()));
	print_attr_xml("to", to_string(msg->to()));
	print_attr_xml("cc", to_string(msg->cc()));
	print_attr_xml("subject", msg->subject());
	g_print("\t\t<date>%u</date>\n", (unsigned)msg->date());
	g_print("\t\t<size>%u</size>\n", (unsigned)msg->size());
	print_attr_xml("msgid", msg->message_id());
	print_attr_xml("path", msg->path());
	print_attr_xml("maildir", msg->maildir());
	g_print("\t</message>\n");

	return true;
}

static OutputFunc
get_output_func(const Options& opts, GError** err)
{
	if (!opts.find.exec.empty())
		return exec_cmd;

	switch (opts.find.format) {
	case Format::Links: return output_link;
	case Format::Plain: return output_plain;
	case Format::Xml: return output_xml;
	case Format::Sexp: return output_sexp;
	case Format::Json: return output_json;
	default: g_return_val_if_reached(NULL); return NULL;
	}


}

static Result<void>
output_query_results(const QueryResults& qres, const Options& opts)
{
	GError* err{};
	const auto output_func{get_output_func(opts, &err)};
	if (!output_func)
		return Err(Error::Code::Query, &err, "failed to find output function");

	bool rv{true};
	output_func(Nothing, FirstOutput, opts, {});

	size_t n{0};
	for (auto&& item : qres) {
		n++;
		auto msg{item.message()};
		if (!msg)
			continue;

		if (msg->changed() < opts.find.after.value_or(0))
			continue;

		rv = output_func(msg,
				 {item.doc_id(),
				  false,
				  false,
				  n == qres.size(), /* last? */
				  item.query_match()},
				 opts,
				 &err);
		if (!rv)
			break;
	}
	output_func(Nothing, LastOutput, opts, {});


	if (rv)
		return Ok();
	else
		return Err(Error::Code::Query, &err, "error in query results output");
}

static Result<void>
process_query(const Store& store, const std::string& expr, const Options& opts)
{
	auto qres{run_query(store, expr, opts)};
	if (!qres)
		return Err(qres.error());

	if (qres->empty())
		return Err(Error::Code::NoMatches,  "no matches for search expression");

	return output_query_results(*qres, opts);
}

Result<void>
Mu::mu_cmd_find(const Store& store, const Options& opts)
{
	auto expr{get_query(opts)};
	if (!expr)
		return Err(expr.error());

	if (opts.find.format == Format::XQuery)
		return print_internal(store, *expr, true, false);
	else if (opts.find.format == Format::MQuery)
		return print_internal(store, *expr, false, opts.verbose);
	else
		return process_query(store, *expr, opts);
}
