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
#include "mu-runtime.hh"
#include "message/mu-message.hh"

#include "utils/mu-option.hh"
#include "utils/mu-util.h"

#include "mu-cmd.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

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
				      const MuConfig*, GError**)>;

static Result<void>
print_internal(const Store&       store,
	       const std::string& expr,
	       gboolean           xapian,
	       gboolean           warn)
{
	std::cout << store.parse_query(expr, xapian) << "\n";
	return Ok();
}

static Result<QueryResults>
run_query(const Store& store, const std::string& expr, const MuConfig* opts)
{
	const auto sortfield{field_from_name(opts->sortfield ? opts->sortfield : "")};
	if (!sortfield && opts->sortfield)
		return Err(Error::Code::InvalidArgument,
			   "invalid sort field: '%s'", opts->sortfield);

	Mu::QueryFlags qflags{QueryFlags::None};
	if (opts->reverse)
		qflags |= QueryFlags::Descending;
	if (opts->skip_dups)
		qflags |= QueryFlags::SkipDuplicates;
	if (opts->include_related)
		qflags |= QueryFlags::IncludeRelated;
	if (opts->threads)
		qflags |= QueryFlags::Threading;

	return store.run_query(expr, sortfield.value_or(field_from_id(Field::Id::Date)).id,
			       qflags, opts->maxnum);
}

static gboolean
exec_cmd(const Option<Message>& msg, const OutputInfo& info, const MuConfig* opts, GError** err)
{
	if (!msg)
		return TRUE;

	gint     status;
	char *   cmdline, *escpath;
	gboolean rv;

	escpath = g_shell_quote(msg->path().c_str());
	cmdline = g_strdup_printf("%s %s", opts->exec, escpath);

	rv = g_spawn_command_line_sync(cmdline, NULL, NULL, &status, err);

	g_free(cmdline);
	g_free(escpath);

	return rv;
}

static gchar*
resolve_bookmark(const MuConfig* opts, GError** err)
{
	MuBookmarks* bm;
	char*        val;
	const gchar* bmfile;

	bmfile = mu_runtime_path(MU_RUNTIME_PATH_BOOKMARKS);
	bm     = mu_bookmarks_new(bmfile);
	if (!bm) {
		g_set_error(err,
			    MU_ERROR_DOMAIN,
			    MU_ERROR_FILE_CANNOT_OPEN,
			    "failed to open bookmarks file '%s'",
			    bmfile);
		return FALSE;
	}

	val = (gchar*)mu_bookmarks_lookup(bm, opts->bookmark);
	if (!val)
		g_set_error(err,
			    MU_ERROR_DOMAIN,
			    MU_ERROR_NO_MATCHES,
			    "bookmark '%s' not found",
			    opts->bookmark);
	else
		val = g_strdup(val);

	mu_bookmarks_destroy(bm);
	return val;
}

static Result<std::string>
get_query(const MuConfig* opts)
{
	GError *err{};
	gchar *query, *bookmarkval;

	/* params[0] is 'find', actual search params start with [1] */
	if (!opts->bookmark && !opts->params[1])
		return Err(Error::Code::InvalidArgument, "error in parameters");

	bookmarkval = {};
	if (opts->bookmark) {
		bookmarkval = resolve_bookmark(opts, &err);
		if (!bookmarkval)
			return Err(Error::Code::Command, &err,
				   "failed to resolve bookmark");
	}

	query = g_strjoinv(" ", &opts->params[1]);
	if (bookmarkval) {
		gchar* tmp;
		tmp = g_strdup_printf("%s %s", bookmarkval, query);
		g_free(query);
		query = tmp;
	}
	g_free(bookmarkval);

	return Ok(to_string_gchar(std::move(query)));
}

static bool
prepare_links(const MuConfig* opts, GError** err)
{
	/* note, mu_maildir_mkdir simply ignores whatever part of the
	 * mail dir already exists */
	if (auto&& res = maildir_mkdir(opts->linksdir, 0700, true); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	if (!opts->clearlinks)
		return false;

	if (auto&& res = maildir_clear_links(opts->linksdir); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	return true;
}

static bool
output_link(const Option<Message>& msg, const OutputInfo& info, const MuConfig* opts, GError** err)
{
	if (info.header)
		return prepare_links(opts, err);
	else if (info.footer)
		return true;

	if (auto&& res = maildir_link(msg->path(), opts->linksdir); !res) {
		res.error().fill_g_error(err);
		return false;
	}

	return true;
}

static void
ansi_color_maybe(Field::Id field_id, gboolean color)
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
ansi_reset_maybe(Field::Id field_id, gboolean color)
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
print_summary(const Message& msg, const MuConfig* opts)
{
	const auto body{msg.body_text()};
	if (!body)
		return;

	const auto summ{to_string_opt_gchar(
			mu_str_summarize(body->c_str(),
					 opts->summary_len))};

	g_print("Summary: ");
	mu_util_fputs_encoded(summ ? summ->c_str() : "<none>", stdout);
	g_print("\n");
}

static void
thread_indent(const QueryMatch& info, const MuConfig* opts)
{
	const auto is_root{any_of(info.flags & QueryMatch::Flags::Root)};
	const auto first_child{any_of(info.flags & QueryMatch::Flags::First)};
	const auto last_child{any_of(info.flags & QueryMatch::Flags::Last)};
	const auto empty_parent{any_of(info.flags & QueryMatch::Flags::Orphan)};
	const auto is_dup{any_of(info.flags & QueryMatch::Flags::Duplicate)};
	// const auto is_related{any_of(info.flags & QueryMatch::Flags::Related)};

	/* indent */
	if (opts->debug) {
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
output_plain_fields(const Message& msg, const char* fields,
		    gboolean color, gboolean threads)
{
	const char* myfields;
	int         nonempty;

	g_return_if_fail(fields);

	for (myfields = fields, nonempty = 0; *myfields; ++myfields) {
		const auto field_opt{field_from_shortcut(*myfields)};
		if (!field_opt || (!field_opt->is_value() && !field_opt->is_contact()))
			nonempty += printf("%c", *myfields);

		else {
			ansi_color_maybe(field_opt->id, color);
			nonempty += mu_util_fputs_encoded(
				display_field(msg, field_opt->id).c_str(), stdout);
			ansi_reset_maybe(field_opt->id, color);
		}
	}

	if (nonempty)
		fputs("\n", stdout);
}

static gboolean
output_plain(const Option<Message>& msg, const OutputInfo& info,
	     const MuConfig* opts, GError** err)
{
	if (!msg)
		return true;

	/* we reuse the color (whatever that may be)
	 * for message-priority for threads, too */
	ansi_color_maybe(Field::Id::Priority, !opts->nocolor);
	if (opts->threads && info.match_info)
		thread_indent(*info.match_info, opts);

	output_plain_fields(*msg, opts->fields, !opts->nocolor, opts->threads);

	if (opts->summary_len > 0)
		print_summary(*msg, opts);

	return TRUE;
}

static bool
output_sexp(const Option<Message>& msg, const OutputInfo& info, const MuConfig* opts, GError** err)
{
	if (msg) {

		if (const auto sexp{msg->cached_sexp()}; !sexp.empty())
			fputs(sexp.c_str(), stdout);
		else
			fputs(msg->to_sexp().to_sexp_string().c_str(), stdout);

		fputs("\n", stdout);
	}

	return true;
}

static bool
output_json(const Option<Message>& msg, const OutputInfo& info, const MuConfig* opts, GError** err)
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
		msg->to_sexp().to_json_string().c_str(),
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
output_xml(const Option<Message>& msg, const OutputInfo& info, const MuConfig* opts, GError** err)
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
get_output_func(const MuConfig* opts, GError** err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_LINKS: return output_link;
	case MU_CONFIG_FORMAT_EXEC: return exec_cmd;
	case MU_CONFIG_FORMAT_PLAIN: return output_plain;
	case MU_CONFIG_FORMAT_XML: return output_xml;
	case MU_CONFIG_FORMAT_SEXP: return output_sexp;
	case MU_CONFIG_FORMAT_JSON: return output_json;

	default: g_return_val_if_reached(NULL); return NULL;
	}
}

static Result<void>
output_query_results(const QueryResults& qres, const MuConfig* opts)
{
	GError* err{};
	const auto output_func{get_output_func(opts, &err)};
	if (!output_func)
		return Err(Error::Code::Query, &err, "failed to find output function");

	gboolean rv{true};
	output_func(Nothing, FirstOutput, opts, {});

	size_t n{0};
	for (auto&& item : qres) {
		n++;
		auto msg{item.message()};
		if (!msg)
			continue;

		if (opts->after != 0 && msg->changed() < opts->after)
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
process_query(const Store& store, const std::string& expr, const MuConfig* opts)
{
	auto qres{run_query(store, expr, opts)};
	if (!qres)
		return Err(qres.error());

	if (qres->empty())
		return Err(Error::Code::NoMatches,  "no matches for search expression");

	return output_query_results(*qres, opts);
}

static Result<void>
execute_find(const Store& store, const MuConfig* opts)
{
	auto expr{get_query(opts)};
	if (!expr)
		return Err(expr.error());

	if (opts->format == MU_CONFIG_FORMAT_XQUERY)
		return print_internal(store, *expr, TRUE, FALSE);
	else if (opts->format == MU_CONFIG_FORMAT_MQUERY)
		return print_internal(store, *expr, FALSE, opts->verbose);
	else
		return process_query(store, *expr, opts);
}

static gboolean
format_params_valid(const MuConfig* opts, GError** err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_EXEC: break;
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP:
	case MU_CONFIG_FORMAT_JSON:
	case MU_CONFIG_FORMAT_LINKS:
	case MU_CONFIG_FORMAT_XML:
	case MU_CONFIG_FORMAT_XQUERY:
	case MU_CONFIG_FORMAT_MQUERY:
		if (opts->exec) {
			mu_util_g_set_error(err,
					    MU_ERROR_IN_PARAMETERS,
					    "--exec and --format cannot be combined");
			return FALSE;
		}
		break;
	default:
		mu_util_g_set_error(err,
				    MU_ERROR_IN_PARAMETERS,
				    "invalid output format %s",
				    opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	if (opts->format == MU_CONFIG_FORMAT_LINKS && !opts->linksdir) {
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS, "missing --linksdir argument");
		return FALSE;
	}

	if (opts->linksdir && opts->format != MU_CONFIG_FORMAT_LINKS) {
		mu_util_g_set_error(err,
				    MU_ERROR_IN_PARAMETERS,
				    "--linksdir is only valid with --format=links");
		return FALSE;
	}

	return TRUE;
}

static gboolean
query_params_valid(const MuConfig* opts, GError** err)
{
	const gchar* xpath;

	if (!opts->params[1]) {
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS, "missing query");
		return FALSE;
	}

	xpath = mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB);
	if (mu_util_check_dir(xpath, TRUE, FALSE))
		return TRUE;

	mu_util_g_set_error(err,
			    MU_ERROR_FILE_CANNOT_READ,
			    "'%s' is not a readable Xapian directory",
			    xpath);
	return FALSE;
}

Result<void>
Mu::mu_cmd_find(const Store& store, const MuConfig* opts)
{
	g_return_val_if_fail(opts, Err(Error::Code::Internal, "no opts"));
	g_return_val_if_fail(opts->cmd == MU_CONFIG_CMD_FIND, Err(Error::Code::Internal,
								  "wrong command"));
	MuConfig myopts{*opts};

	if (myopts.exec)
		myopts.format = MU_CONFIG_FORMAT_EXEC; /* pseudo format */

	GError *err{};
	if (!query_params_valid(&myopts, &err) || !format_params_valid(&myopts, &err))
		return Err(Error::Code::InvalidArgument, &err, "invalid argument");
	else
		return execute_find(store, &myopts);
}
