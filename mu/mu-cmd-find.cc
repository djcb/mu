 /*
** Copyright (C) 2008-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <sys/wait.h>

#include "message/mu-message.hh"
#include "mu-maildir.hh"
#include "mu-query-match-deciders.hh"
#include "mu-query.hh"
#include "mu-query-macros.hh"
#include "mu-query-parser.hh"
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

using OutputFunc = std::function<Result<void>(const Option<Message>& msg, const OutputInfo&,
					      const Options&)>;

using Format = Options::Find::Format;

static Result<void>
analyze_query_expr(const Store& store, const std::string& expr, const Options& opts)
{
	auto print_item=[&](auto&&title, auto&&val) {
		const auto blue{opts.nocolor  ? "" : MU_COLOR_BLUE};
		const auto green{opts.nocolor ? "" : MU_COLOR_GREEN};
		const auto reset{opts.nocolor ? "" : MU_COLOR_DEFAULT};
		mu_println("* {}{}{}:\n  {}{}{}", blue, title, reset, green, val, reset);
	};

	print_item("query", expr);

	const auto pq{parse_query(expr, false/*don't expand*/).to_string()};
	const auto pqx{parse_query(expr, true/*do expand*/).to_string()};

	print_item("parsed query", pq);
	if (pq != pqx)
		print_item("parsed query (expanded)", pqx);

	auto xq{make_xapian_query(store, expr)};
	if (!xq)
		return Err(std::move(xq.error()));

	print_item("Xapian query", xq->get_description());

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

static Result<void>
exec_cmd(const Option<Message>& msg, const OutputInfo& info, const Options& opts)
{
	if (!msg)
		return Ok();

	int wait_status{};
	GError *err{};
	auto cmdline{mu_format("{} {}", opts.find.exec,
			       to_string_gchar(g_shell_quote(msg->path().c_str())))};

	if (!g_spawn_command_line_sync(cmdline.c_str(), {}, {}, &wait_status, &err))
		return Err(Error::Code::File, &err/*consumed*/,
			   "failed to execute shell command");
	else if (WEXITSTATUS(wait_status) != 0)
		return Err(Error::Code::File,
			"shell command exited with exit-code {}",
			   WEXITSTATUS(wait_status));
	return Ok();
}

static Result<std::string>
resolve_bookmark(const Store& store, const Options& opts)
{
	QueryMacros macros{store.config()};
	if (auto&& res{macros.load_bookmarks(opts.runtime_path(RuntimePath::Bookmarks))}; !res)
		return Err(res.error());
	else if (auto&& bm{macros.find_macro(opts.find.bookmark)}; !bm)
		return Err(Error::Code::InvalidArgument, "bookmark '{}' not found",
			   opts.find.bookmark);
	else
		return Ok(std::move(*bm));
}

static Result<std::string>
get_query(const Store& store, const Options& opts)
{
	if (opts.find.bookmark.empty() && opts.find.query.empty())
		return Err(Error::Code::InvalidArgument,
			   "neither bookmark nor query");

	std::string bookmark;
	if (!opts.find.bookmark.empty()) {
		const auto res = resolve_bookmark(store, opts);
		if (!res)
			return Err(std::move(res.error()));
		bookmark = res.value() + " ";
	}

	auto&& query{join(opts.find.query, " ")};
	return Ok(bookmark + query);
}

static Result<void>
prepare_links(const Options& opts)
{
	// XXX: can this be done as part of the option-parsing?
	if (opts.find.linksdir.empty())
		return Err(Error::Code::InvalidArgument, "--linksdir is required");

	/* note, mu_maildir_mkdir simply ignores whatever part of the
	 * mail dir already exists */
	if (auto&& res = maildir_mkdir(opts.find.linksdir, 0700, true); !res)
		return Err(std::move(res.error()));

	if (!opts.find.clearlinks)
		return Ok();

	if (auto&& res = maildir_clear_links(opts.find.linksdir); !res)
		return Err(std::move(res.error()));

	return Ok();
}

static Result<void>
output_link(const Option<Message>& msg, const OutputInfo& info, const Options& opts)
{
	if (info.header)
		return prepare_links(opts);
	else if (info.footer)
		return Ok();

	/* during test, do not create "unique names" (i.e., names with path
	 * hashes), so we get a predictable result */
	const auto unique_names{!g_getenv("MU_TEST")&&!g_test_initialized()};

	if (auto&& res = maildir_link(msg->path(), opts.find.linksdir, unique_names); !res)
		return Err(std::move(res.error()));

	return Ok();
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
		return mu_format("{:%c}",
				 mu_time(msg.document().integer_value(field_id)));
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

	mu_print("Summary: ");
	fputs_encoded(summ, stdout);
	mu_println("");
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

static Result<void>
output_plain(const Option<Message>& msg, const OutputInfo& info,
	     const Options& opts)
{
	if (!msg)
		return Ok();

	/* we reuse the color (whatever that may be)
	 * for message-priority for threads, too */
	ansi_color_maybe(Field::Id::Priority, !opts.nocolor);
	if (opts.find.threads && info.match_info)
		thread_indent(*info.match_info, opts);

	output_plain_fields(*msg, opts.find.fields, !opts.nocolor, opts.find.threads);

	if (opts.find.summary_len)
		print_summary(*msg, opts);

	return Ok();
}

static Result<void>
output_sexp(const Option<Message>& msg, const OutputInfo& info, const Options& opts)
{
	if (msg) {
		if (const auto sexp{msg->sexp()}; !sexp.empty())
			fputs(sexp.to_string().c_str(), stdout);
		else
			fputs(msg->sexp().to_string().c_str(), stdout);
		fputs("\n", stdout);
	}

	return Ok();
}

static Result<void>
output_json(const Option<Message>& msg, const OutputInfo& info, const Options& opts)
{
	if (info.header) {
		mu_println("[");
		return Ok();
	}

	if (info.footer) {
		mu_println("]");
		return Ok();
	}

	if (!msg)
		return Ok();

	mu_println("{}{}", msg->sexp().to_json_string(), info.last ? "" : ",");

	return Ok();
}

static void
print_attr_xml(const std::string& elm, const std::string& str)
{
	if (str.empty())
		return; /* empty: don't include */

	auto&& esc{to_string_opt_gchar(g_markup_escape_text(str.c_str(), -1))};
	mu_println("\t\t<{}>{}</{}>", elm, esc.value_or(""), elm);
}

static Result<void>
output_xml(const Option<Message>& msg, const OutputInfo& info, const Options& opts)
{
	if (info.header) {
		mu_println("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>");
		mu_println("<messages>");
		return Ok();
	}

	if (info.footer) {
		mu_println("</messages>");
		return Ok();
	}

	mu_println("\t<message>");
	print_attr_xml("from", to_string(msg->from()));
	print_attr_xml("to", to_string(msg->to()));
	print_attr_xml("cc", to_string(msg->cc()));
	print_attr_xml("subject", msg->subject());
	mu_println("\t\t<date>{}</date>", (unsigned)msg->date());
	mu_println("\t\t<size>{}</size>", (unsigned)msg->size());
	print_attr_xml("msgid", msg->message_id());
	print_attr_xml("path", msg->path());
	print_attr_xml("maildir", msg->maildir());
	mu_println("\t</message>");

	return Ok();
}

static OutputFunc
get_output_func(const Options& opts)
{
	if (!opts.find.exec.empty())
		return exec_cmd;

	switch (opts.find.format) {
	case Format::Links:
		return output_link;
	case Format::Plain:
		return output_plain;
	case Format::Xml:
		return output_xml;
	case Format::Sexp:
		return output_sexp;
	case Format::Json:
		return output_json;
	default:
		throw Error(Error::Code::Internal,
			    "invalid format {}",
			    static_cast<size_t>(opts.find.format));
	}
}

static Result<void>
output_query_results(const QueryResults& qres, const Options& opts)
{
	GError* err{};
	const auto output_func{get_output_func(opts)};
	if (!output_func)
		return Err(Error::Code::Query, &err, "failed to find output function");

	if (auto&& res = output_func(Nothing, FirstOutput, opts); !res)
		return Err(std::move(res.error()));

	size_t n{0};
	for (auto&& item : qres) {
		n++;
		auto msg{item.message()};
		if (!msg)
			continue;

		if (msg->changed() < opts.find.after.value_or(0))
			continue;

		if (auto&& res = output_func(msg,
					     {item.doc_id(),
					      false,
					      false,
					      n == qres.size(), /* last? */
					      item.query_match()},
					     opts); !res)
			return Err(std::move(res.error()));
	}

	if (auto&& res{output_func(Nothing, LastOutput, opts)}; !res)
		return Err(std::move(res.error()));
	else
		return Ok();
}

static Result<void>
process_store_query(const Store& store, const std::string& expr, const Options& opts)
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
	auto expr{get_query(store, opts)};
	if (!expr)
		return Err(expr.error());

	if (opts.find.analyze)
		return analyze_query_expr(store, *expr, opts);
	else
		return process_store_query(store, *expr, opts);
}



#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"


/* tests for the command line interface, uses testdir2 */

static std::string test_mu_home;

auto count_nl(const std::string& s)->size_t {
	size_t n{};
	for (auto&& c: s)
		if (c == '\n')
			++n;
	return n;
}

static size_t
search_func(const std::string& expr, size_t expected)
{
	auto res = run_command({MU_PROGRAM, "find", "--muhome", test_mu_home, expr});
	assert_valid_result(res);

	/* we expect zero lines of error output if there is a match; otherwise
	 * there should be one line 'No matches found' */
	if (res->exit_code != 0) {
		g_assert_cmpuint(res->exit_code, ==, 2); // no match
		g_assert_true(res->standard_out.empty());
		g_assert_cmpuint(count_nl(res->standard_err), ==, 1);
		return 0;
	}

	return count_nl(res->standard_out);
}

#define search(Q,EXP) do {				\
	g_assert_cmpuint(search_func(Q, EXP), ==, EXP); \
} while(0)


static void
test_mu_find_empty_query(void)
{
	search("\"\"", 14);
}

static void
test_mu_find_01(void)
{
	search("f:john fruit", 1);
	search("f:soc@example.com", 1);
	search("t:alki@example.com", 1);
	search("t:alcibiades", 1);
	search("http emacs", 1);
	search("f:soc@example.com OR f:john", 2);
	search("f:soc@example.com OR f:john OR t:edmond", 3);
	search("t:julius", 1);
	search("s:dude", 1);
	search("t:dantès", 1);
}

/* index testdir2, and make sure it adds two documents */
static void
test_mu_find_02(void)
{
	search("bull", 1);
	search("g:x", 0);
	search("flag:encrypted", 0);
	search("flag:attach", 1);

	search("i:3BE9E6535E0D852173@emss35m06.us.lmco.com", 1);
}

static void
test_mu_find_file(void)
{
	search("file:sittingbull.jpg", 1);
	search("file:custer.jpg", 1);
	search("file:custer.*", 1);
	search("j:sit*", 1);
}

static void
test_mu_find_mime(void)
{
	search("mime:image/jpeg", 1);
	search("mime:text/plain", 14);
	search("y:text*", 14);
	search("y:image*", 1);
	search("mime:message/rfc822", 2);
}

static void
test_mu_find_text_in_rfc822(void)
{
	search("embed:dancing", 1);
	search("e:curious", 1);
	search("embed:with", 2);
	search("e:karjala", 0);
	search("embed:navigation", 1);
}

static void
test_mu_find_maildir_special(void)
{
	search("\"maildir:/wOm_bàT\"", 3);
	search("\"maildir:/wOm*\"", 3);
	search("\"maildir:/wOm_*\"", 3);
	search("\"maildir:wom_bat\"", 0);
	search("\"maildir:/wombat\"", 0);
	search("subject:atoms", 1);
	search("\"maildir:/wom_bat\" subject:atoms", 1);
}


/* some more tests */

static void
test_mu_find_wrong_muhome()
{
	auto res = run_command({MU_PROGRAM, "find", "--muhome",
			join_paths("/foo", "bar", "nonexistent"), "f:socrates"});
	assert_valid_result(res);
	g_assert_cmpuint(res->exit_code,==,1); // general error
	g_assert_cmpuint(count_nl(res->standard_err), >, 1);
}

static void
test_mu_find_links(void)
{
	TempDir temp_dir;

	{
		auto res = run_command({MU_PROGRAM, "find", "--muhome", test_mu_home,
				"--format", "links", "--linksdir", temp_dir.path(),
				"mime:message/rfc822"});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out),==,0);
		g_assert_cmpuint(count_nl(res->standard_err),==,0);
	}


	/* furthermore, two symlinks should be there */
	const auto f1{mu_format("{}/cur/rfc822.1", temp_dir)};
	const auto f2{mu_format("{}/cur/rfc822.2", temp_dir)};

	g_assert_cmpuint(determine_dtype(f1.c_str(), true), ==, DT_LNK);
	g_assert_cmpuint(determine_dtype(f2.c_str(), true), ==, DT_LNK);

	/* now we try again, we should get a line of error output,
	 * when we find the first target file already exists */
	{
		auto res = run_command({MU_PROGRAM, "find", "--muhome", test_mu_home,
				"--format", "links", "--linksdir", temp_dir.path(),
				"mime:message/rfc822"});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,1);
		g_assert_cmpuint(count_nl(res->standard_out),==,0);
		g_assert_cmpuint(count_nl(res->standard_err),==,1);
	}

	/* now we try again with --clearlinks, and the we should be
	 * back to 0 errors */
	{
		auto res = run_command({MU_PROGRAM, "find", "--muhome", test_mu_home,
				"--format", "links", "--clearlinks", "--linksdir", temp_dir.path(),
				"mime:message/rfc822"});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out),==,0);
		g_assert_cmpuint(count_nl(res->standard_err),==,0);
	}

	g_assert_cmpuint(determine_dtype(f1.c_str(), true), ==, DT_LNK);
	g_assert_cmpuint(determine_dtype(f2.c_str(), true), ==, DT_LNK);
}

/* some more tests */

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	if (!set_en_us_utf8_locale())
		return 0; /* don't error out... */

	TempDir temp_dir{};
	{
		test_mu_home = temp_dir.path();

		auto res1 = run_command({MU_PROGRAM, "--quiet", "init",
				"--muhome", test_mu_home, "--maildir" , MU_TESTMAILDIR2});
		assert_valid_result(res1);

		auto res2 = run_command({MU_PROGRAM, "--quiet", "index",
				"--muhome", test_mu_home});
		assert_valid_result(res2);
	}

	g_test_add_func("/cmd/find/empty-query", test_mu_find_empty_query);
	g_test_add_func("/cmd/find/01", test_mu_find_01);
	g_test_add_func("/cmd/find/02", test_mu_find_02);
	g_test_add_func("/cmd/find/file", test_mu_find_file);
	g_test_add_func("/cmd/find/mime", test_mu_find_mime);
	g_test_add_func("/cmd/find/links", test_mu_find_links);
	g_test_add_func("/cmd/find/text-in-rfc822", test_mu_find_text_in_rfc822);
	g_test_add_func("/cmd/find/wrong-muhome", test_mu_find_wrong_muhome);
	g_test_add_func("/cmd/find/maildir-special", test_mu_find_maildir_special);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
