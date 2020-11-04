/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg.h"
#include "mu-maildir.h"
#include "mu-query.hh"
#include "mu-msg-iter.h"
#include "mu-bookmarks.h"
#include "mu-runtime.h"

#include "utils/mu-util.h"
#include "utils/mu-str.h"
#include "utils/mu-date.h"

#include "mu-cmd.hh"
#include "mu-threader.hh"

using namespace Mu;

typedef gboolean (OutputFunc) (MuMsg *msg, MuMsgIter *iter,
			       const MuConfig *opts, GError **err);

static gboolean
print_internal (const Query& query, const gchar *expr, gboolean xapian,
		gboolean warn, GError **err)
{
        std::cout << query.parse(expr, xapian) << "\n";
        return TRUE;
}


/* returns MU_MSG_FIELD_ID_NONE if there is an error */
static MuMsgFieldId
sort_field_from_string (const char* fieldstr, GError **err)
{
	MuMsgFieldId mfid;

	mfid = mu_msg_field_id_from_name (fieldstr, FALSE);

	/* not found? try a shortcut */
	if (mfid == MU_MSG_FIELD_ID_NONE &&
	    strlen(fieldstr) == 1)
		mfid = mu_msg_field_id_from_shortcut(fieldstr[0],
						     FALSE);
	if (mfid == MU_MSG_FIELD_ID_NONE)
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
			     "not a valid sort field: '%s'\n", fieldstr);
	return mfid;
}

static MuMsg*
get_message (MuMsgIter *iter, time_t after)
{
	MuMsg *msg;

	if (mu_msg_iter_is_done (iter))
		return NULL;

	msg = mu_msg_iter_get_msg_floating (iter);
	if (!msg)
		return NULL; /* error */

	if (!mu_msg_is_readable (msg)) {
		mu_msg_iter_next (iter);
		return get_message (iter, after);
	}

	if (after != 0 && after > mu_msg_get_timestamp (msg)) {
		mu_msg_iter_next (iter);
		return get_message (iter, after);
	}

	return msg;
}

static MuMsgIter*
run_query (const Query& q, const std::string& expr, const MuConfig *opts, GError **err)
{
	MuMsgIter    *iter;
	MuMsgFieldId  sortid;

	sortid = MU_MSG_FIELD_ID_NONE;
	if (opts->sortfield) {
		sortid = sort_field_from_string (opts->sortfield, err);
		if (sortid == MU_MSG_FIELD_ID_NONE) /* error occurred? */
			return FALSE;
	}

	Mu::Query::Flags qflags{Query::Flags::None};
	if (opts->reverse)
		qflags |= Query::Flags::Descending;
	if (opts->skip_dups)
		qflags |= Query::Flags::SkipDups;
	if (opts->include_related)
		qflags |= Query::Flags::IncludeRelated;
	if (opts->threads)
		qflags |= Query::Flags::Threading;

	return q.run(expr, sortid, qflags, opts->maxnum, err);
}

static gboolean
exec_cmd (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts,  GError **err)
{
	gint status;
	char *cmdline, *escpath;
	gboolean rv;

	escpath = g_shell_quote (mu_msg_get_path (msg));
	cmdline = g_strdup_printf ("%s %s", opts->exec, escpath);

	rv = g_spawn_command_line_sync (cmdline, NULL, NULL, &status, err);

	g_free (cmdline);
	g_free (escpath);

	return rv;
}

static gchar*
resolve_bookmark (const MuConfig *opts, GError **err)
{
	MuBookmarks *bm;
	char* val;
	const gchar *bmfile;

	bmfile = mu_runtime_path (MU_RUNTIME_PATH_BOOKMARKS);
	bm = mu_bookmarks_new (bmfile);
	if (!bm) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE_CANNOT_OPEN,
			     "failed to open bookmarks file '%s'", bmfile);
		return FALSE;
	}

	val = (gchar*)mu_bookmarks_lookup (bm, opts->bookmark);
	if (!val)
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_NO_MATCHES,
			     "bookmark '%s' not found", opts->bookmark);
	else
		val = g_strdup (val);

	mu_bookmarks_destroy (bm);
	return val;
}

static gchar*
get_query (const MuConfig *opts, GError **err)
{
	gchar	*query, *bookmarkval;

	/* params[0] is 'find', actual search params start with [1] */
	if (!opts->bookmark && !opts->params[1]) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
			     "error in parameters");
		return NULL;
	}

	bookmarkval = NULL;
	if (opts->bookmark) {
		bookmarkval = resolve_bookmark (opts, err);
		if (!bookmarkval)
			return NULL;
	}

	query = g_strjoinv (" ", &opts->params[1]);
	if (bookmarkval) {
		gchar *tmp;
		tmp = g_strdup_printf ("%s %s", bookmarkval, query);
		g_free (query);
		query = tmp;
	}

	g_free (bookmarkval);

	return query;
}

static Mu::Query
get_query_obj (const Store& store, GError **err)
{
	const auto count{store.size()};

	if (count == (unsigned)-1)
                throw Mu::Error(Error::Code::Store, "invalid store");

	if (count == 0)
                throw Mu::Error(Error::Code::Store, "store is empty");

	return Mu::Query{store};
}

static gboolean
prepare_links (const MuConfig *opts, GError **err)
{
	/* note, mu_maildir_mkdir simply ignores whatever part of the
	 * mail dir already exists */

	if (!mu_maildir_mkdir (opts->linksdir, 0700, TRUE, err)) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_MKDIR,
				     "error creating %s", opts->linksdir);
		return FALSE;
	}

	if (opts->clearlinks &&
	    !mu_maildir_clear_links (opts->linksdir, err)) {
			mu_util_g_set_error (err, MU_ERROR_FILE,
					     "error clearing links under %s",
					     opts->linksdir);
			return FALSE;
	}

	return TRUE;
}

static gboolean
output_link (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts,  GError **err)
{
	if (mu_msg_iter_is_first (iter) && !prepare_links (opts, err))
		return FALSE;

	return mu_maildir_link (mu_msg_get_path (msg),
				opts->linksdir, err);
}

static void
ansi_color_maybe (MuMsgFieldId mfid, gboolean color)
{
	const char* ansi;

	if (!color)
		return; /* nothing to do */

	switch (mfid) {

	case MU_MSG_FIELD_ID_FROM:
		ansi = MU_COLOR_CYAN; break;

	case MU_MSG_FIELD_ID_TO:
	case MU_MSG_FIELD_ID_CC:
	case MU_MSG_FIELD_ID_BCC:
		ansi = MU_COLOR_BLUE; break;

	case MU_MSG_FIELD_ID_SUBJECT:
		ansi = MU_COLOR_GREEN; break;

	case MU_MSG_FIELD_ID_DATE:
		ansi = MU_COLOR_MAGENTA; break;

	default:
		if (mu_msg_field_type(mfid) == MU_MSG_FIELD_TYPE_STRING)
			ansi = MU_COLOR_YELLOW;
		else
			ansi = MU_COLOR_RED;
	}

	fputs (ansi, stdout);
}

static void
ansi_reset_maybe (MuMsgFieldId mfid, gboolean color)
{
	if (!color)
		return; /* nothing to do */

	fputs (MU_COLOR_DEFAULT, stdout);

}

static const char*
field_string_list (MuMsg *msg, MuMsgFieldId mfid)
{
	char *str;
	const GSList *lst;
	static char buf[80];

	lst = mu_msg_get_field_string_list (msg, mfid);
	if (!lst)
		return NULL;

	str = mu_str_from_list (lst, ',');
	if (str) {
		strncpy (buf, str, sizeof(buf)-1);
		buf[sizeof(buf)-1]='\0';
		g_free (str);
		return buf;
	}

	return NULL;
}

static const char*
display_field (MuMsg *msg, MuMsgFieldId mfid)
{
	gint64 val;

	switch (mu_msg_field_type(mfid)) {
	case MU_MSG_FIELD_TYPE_STRING: {
		const gchar *str;
		str = mu_msg_get_field_string (msg, mfid);
		return str ? str : "";
	}
	case MU_MSG_FIELD_TYPE_INT:

		if (mfid == MU_MSG_FIELD_ID_PRIO) {
			val = mu_msg_get_field_numeric (msg, mfid);
			return mu_msg_prio_name ((MuMsgPrio)val);
 		} else if (mfid == MU_MSG_FIELD_ID_FLAGS) {
			val = mu_msg_get_field_numeric (msg, mfid);
			return mu_str_flags_s ((MuFlags)val);
		} else  /* as string */
			return mu_msg_get_field_string (msg, mfid);

	case MU_MSG_FIELD_TYPE_TIME_T:
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_date_str_s ("%c", (time_t)val);

	case MU_MSG_FIELD_TYPE_BYTESIZE:
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_str_size_s ((unsigned)val);
	case MU_MSG_FIELD_TYPE_STRING_LIST: {
		const char *str;
		str = field_string_list (msg, mfid);
		return str ? str : "";
	}
	default:
		g_return_val_if_reached (NULL);
	}
}

static void
print_summary (MuMsg *msg, const MuConfig *opts)
{
	const char* body;
	char *summ;
	MuMsgOptions msgopts;

	msgopts = mu_config_get_msg_options (opts);
	body = mu_msg_get_body_text(msg, msgopts);

	if (body)
		summ = mu_str_summarize (body, (unsigned)opts->summary_len);
	else
		summ = NULL;

	g_print ("Summary: ");
	mu_util_fputs_encoded (summ ? summ : "<none>", stdout);
	g_print ("\n");

	g_free (summ);
}

static void
thread_indent (MuMsgIter *iter)
{
	const MuMsgIterThreadInfo *ti;
	const char* threadpath;
	int i;
	gboolean is_root, first_child, empty_parent, is_dup;

	ti = mu_msg_iter_get_thread_info (iter);
	if (!ti) {
		g_printerr ("cannot get thread-info for message %u\n",
                            mu_msg_iter_get_docid (iter));
		return;
	}

	threadpath = ti->threadpath;
	/* fputs (threadpath, stdout); */
	/* fputs ("  ", stdout); */

	is_root      = ti->prop & MU_MSG_ITER_THREAD_PROP_ROOT;
	first_child  = ti->prop & MU_MSG_ITER_THREAD_PROP_FIRST_CHILD;
	empty_parent = ti->prop & MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT;
	is_dup       = ti->prop & MU_MSG_ITER_THREAD_PROP_DUP;

	/* FIXME: count the colons... */
	for (i = 0; *threadpath; ++threadpath)
		i += (*threadpath == ':') ? 1 : 0;

	/* indent */
	while (i --> 0)
		fputs ("  ", stdout);

	if (!is_root) {
		fputs (first_child ? "`" : "|", stdout);
		fputs (empty_parent ? "*> " : is_dup ? "=> " : "-> ", stdout);
	}
}

static void
output_plain_fields (MuMsg *msg, const char *fields,
		     gboolean color, gboolean threads)
{
	const char*	myfields;
	int		nonempty;

	g_return_if_fail (fields);

	for (myfields = fields, nonempty = 0; *myfields; ++myfields) {

		MuMsgFieldId mfid;
		mfid =	mu_msg_field_id_from_shortcut (*myfields, FALSE);

		if (mfid == MU_MSG_FIELD_ID_NONE ||
		    (!mu_msg_field_xapian_value (mfid) &&
		     !mu_msg_field_xapian_contact (mfid)))
		  nonempty += printf ("%c", *myfields);

		else {
			ansi_color_maybe (mfid, color);
			nonempty += mu_util_fputs_encoded
			  (display_field (msg, mfid), stdout);
			ansi_reset_maybe (mfid, color);
		}
	}

	if (nonempty)
		fputs ("\n", stdout);
}

static gboolean
output_plain (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts, GError **err)
{
	/* we reuse the color (whatever that may be)
	 * for message-priority for threads, too */
	ansi_color_maybe (MU_MSG_FIELD_ID_PRIO, !opts->nocolor);
	if (opts->threads)
		thread_indent (iter);

	output_plain_fields (msg, opts->fields, !opts->nocolor, opts->threads);

	if (opts->summary_len > 0)
		print_summary (msg, opts);

	return TRUE;
}


static std::string
to_string (const Mu::Sexp& sexp, bool color, size_t level = 0)
{
        Mu::MaybeAnsi col{color};
        using Color = Mu::MaybeAnsi::Color;

        // clang/libc++ don't allow constexpr here
        const std::array<Color, 6> rainbow = {
                Color::BrightBlue, Color::Green, Color::Yellow,
                Color::Magenta,    Color::Cyan,  Color::BrightGreen,
        };

        std::stringstream sstrm;

        switch (sexp.type()) {
        case Sexp::Type::List: {
                const auto bracecol{col.fg(rainbow[level % rainbow.size()])};
                sstrm << bracecol << "(";

                bool first{true};
                for (auto&& child : sexp.list()) {
                        sstrm << (first ? "" : " ")
                              << to_string(child , color, level + 1);
                        first = false;
                }
                sstrm << bracecol << ")";
                break;
        }
        case Sexp::Type::String:
                sstrm << col.fg(Color::BrightCyan) << Mu::quote(sexp.value())
                      << col.reset();
                break;
        case Sexp::Type::Number:
                sstrm << col.fg(Color::BrightMagenta) << sexp.value()
                      << col.reset();
                break;
        case Sexp::Type::Symbol:
                sstrm << (col.fg(sexp.value().at(0) == ':' ? Color::BrightGreen : Color::BrightBlue))
                      << sexp.value() << col.reset();
                break;
        default:
                throw std::logic_error ("invalid type");
        }

        return sstrm.str();
}


static gboolean
output_sexp (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts, GError **err)
{
	const auto *ti{opts->threads ? mu_msg_iter_get_thread_info (iter) : NULL};
        const auto sexp{Mu::msg_to_sexp(msg , mu_msg_iter_get_docid (iter), ti,
                                        MU_MSG_OPTION_HEADERS_ONLY)};
	fputs (to_string(sexp, !opts->nocolor).c_str(), stdout);
        fputs ("\n", stdout);

	return TRUE;
}

static gboolean
output_json (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts, GError **err)
{
	const MuMsgIterThreadInfo	*ti;
	char				*s;

	if (mu_msg_iter_is_first(iter))
		g_print ("[\n");

	ti = opts->threads ? mu_msg_iter_get_thread_info (iter) : NULL;
	s  = mu_msg_to_json (msg, mu_msg_iter_get_docid (iter),
                             ti, MU_MSG_OPTION_HEADERS_ONLY);
	fputs (s, stdout);
	g_free (s);

	if (mu_msg_iter_is_last(iter))
		fputs("]\n", stdout);
	else
		fputs (",\n", stdout);

	return TRUE;
}

static void
print_attr_xml (const char* elm, const char *str)
{
	gchar *esc;

	if (mu_str_is_empty(str))
		return; /* empty: don't include */

	esc = g_markup_escape_text (str, -1);
	g_print ("\t\t<%s>%s</%s>\n", elm, esc, elm);
	g_free (esc);
}

static gboolean
output_xml (MuMsg *msg, MuMsgIter *iter, const MuConfig *opts, GError **err)
{
	if (mu_msg_iter_is_first(iter)) {
		g_print ("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
		g_print ("<messages>\n");
	}

	g_print ("\t<message>\n");
	print_attr_xml ("from", mu_msg_get_from (msg));
	print_attr_xml ("to", mu_msg_get_to (msg));
	print_attr_xml ("cc", mu_msg_get_cc (msg));
	print_attr_xml ("subject", mu_msg_get_subject (msg));
	g_print ("\t\t<date>%u</date>\n",
		 (unsigned)mu_msg_get_date (msg));
	g_print ("\t\t<size>%u</size>\n", (unsigned)mu_msg_get_size (msg));
	print_attr_xml ("msgid", mu_msg_get_msgid (msg));
	print_attr_xml ("path", mu_msg_get_path (msg));
	print_attr_xml ("maildir", mu_msg_get_maildir (msg));
	g_print ("\t</message>\n");

	if (mu_msg_iter_is_last(iter))
		g_print ("</messages>\n");

	return TRUE;
}

static OutputFunc*
get_output_func (const MuConfig *opts, GError **err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_LINKS: return output_link;
	case MU_CONFIG_FORMAT_EXEC:  return exec_cmd;
	case MU_CONFIG_FORMAT_PLAIN: return output_plain;
	case MU_CONFIG_FORMAT_XML:   return output_xml;
	case MU_CONFIG_FORMAT_SEXP:  return output_sexp;
	case MU_CONFIG_FORMAT_JSON:  return output_json;

	default:
		g_return_val_if_reached (NULL);
		return NULL;
	}
}

static gboolean
output_query_results (MuMsgIter *iter, const MuConfig *opts, GError **err)
{
	int		 count;
	gboolean	 rv;
	OutputFunc	*output_func;

	output_func = get_output_func (opts, err);
	if (!output_func)
		return FALSE;

	for (count = 0, rv = TRUE; !mu_msg_iter_is_done(iter);
	     mu_msg_iter_next (iter)) {

		MuMsg *msg;

		if (count == opts->maxnum)
			break;
		msg = get_message (iter, opts->after);
		if (!msg)
			break;
		/* { */
		/* 	const char* thread_id; */
		/* 	thread_id = mu_msg_iter_get_thread_id (iter); */
		/* 	g_print ("%s ", thread_id ? thread_id : "<none>"); */

		/* } */
		rv = output_func (msg, iter, opts, err);
		if (!rv)
			break;
		else
			++count;
	}

	if (rv && count == 0) {
		mu_util_g_set_error (err, MU_ERROR_NO_MATCHES,
				     "no matches for search expression");
		return FALSE;
	}

	return rv;
}

static gboolean
process_query (const Query& q, const gchar *expr, const MuConfig *opts, GError **err)
{
	gboolean rv;

	auto iter = run_query (q, expr, opts, err);
	if (!iter)
		return FALSE;

	rv = output_query_results (iter, opts, err);
	mu_msg_iter_destroy (iter);

	return rv;
}

static gboolean
execute_find (const Store& store, const MuConfig *opts, GError **err)
{
	auto q{get_query_obj (store, err)};

	auto expr{get_query (opts, err)};
	if (!expr)
		return FALSE;

	if (opts->format == MU_CONFIG_FORMAT_XQUERY)
		return print_internal (q, expr, TRUE, FALSE, err);
	else if (opts->format == MU_CONFIG_FORMAT_MQUERY)
		return print_internal (q, expr, FALSE,
				     opts->verbose, err);
	else
		return process_query (q, expr, opts, err);
}

static gboolean
format_params_valid (const MuConfig *opts, GError **err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_EXEC:
		break;
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP:
	case MU_CONFIG_FORMAT_JSON:
	case MU_CONFIG_FORMAT_LINKS:
	case MU_CONFIG_FORMAT_XML:
	case MU_CONFIG_FORMAT_XQUERY:
	case MU_CONFIG_FORMAT_MQUERY:
		if (opts->exec) {
			mu_util_g_set_error
				(err, MU_ERROR_IN_PARAMETERS,
				 "--exec and --format cannot be combined");
			return FALSE;
		}
		break;
	default:  mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				       "invalid output format %s",
			 opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	if (opts->format == MU_CONFIG_FORMAT_LINKS && !opts->linksdir) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "missing --linksdir argument");
		return FALSE;
	}

	if (opts->linksdir && opts->format != MU_CONFIG_FORMAT_LINKS) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
			 "--linksdir is only valid with --format=links");
		return FALSE;
	}

	return TRUE;
}

static gboolean
query_params_valid (const MuConfig *opts, GError **err)
{
	const gchar *xpath;

	if (!opts->params[1]) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "missing query");
		return FALSE;
	}

	xpath = mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB);
	if (mu_util_check_dir (xpath, TRUE, FALSE))
		return TRUE;

	mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_READ,
			     "'%s' is not a readable Xapian directory",
			     xpath);
	return FALSE;
}

MuError
mu_cmd_find (const Store& store, const MuConfig *opts, GError **err)
{
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_FIND,
			      MU_ERROR_INTERNAL);

        MuConfig myopts{*opts};

	if (myopts.exec)
		myopts.format = MU_CONFIG_FORMAT_EXEC; /* pseudo format */

	if (!query_params_valid (&myopts, err) ||
	    !format_params_valid(&myopts, err))
		return MU_G_ERROR_CODE (err);

	if (!execute_find (store, &myopts, err))
		return MU_G_ERROR_CODE(err);
	else
		return MU_OK;
}
