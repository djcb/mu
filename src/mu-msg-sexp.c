/*
** Copyright (C) 2011  Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string.h>

#include "mu-str.h"
#include "mu-msg.h"
#include "mu-maildir.h"

static void
append_sexp_attr (GString *gstr, const char* elm, const char *str)
{
	gchar *esc;
	
	if (!str || strlen(str) == 0)
		return; /* empty: don't include */

	esc = mu_str_escape_c_literal (str, TRUE);
	
	g_string_append_printf (gstr, "\t:%s %s\n", elm, esc);
	g_free (esc);
}


struct _ContactData {
	gboolean from, to, cc, bcc;
	GString *gstr;
}; 
typedef struct _ContactData ContactData;

static gchar*
get_name_addr_pair (MuMsgContact *c)
{
	gchar *name, *addr, *pair;

	name = (char*)mu_msg_contact_name(c);
	addr = (char*)mu_msg_contact_address(c);

	name = name ? mu_str_escape_c_literal (name, TRUE) : NULL;
	addr = addr ? mu_str_escape_c_literal (addr, TRUE) : NULL;

	pair = g_strdup_printf ("(%s . %s)",
				name ? name : "nil",
				addr ? addr : "nil");
	g_free (name);
	g_free (addr);

	return pair;
}


static gboolean
each_contact (MuMsgContact *c, ContactData *cdata)
{
	char *pair;
	
	pair = get_name_addr_pair (c);
	
	if (mu_msg_contact_type (c) == MU_MSG_CONTACT_TYPE_FROM) {
		if (!cdata->from)
			g_string_append (cdata->gstr, "\t:from (");
		g_string_append (cdata->gstr, pair);
		cdata->from = TRUE;
	}

	if (mu_msg_contact_type (c) == MU_MSG_CONTACT_TYPE_TO) {
		if (!cdata->to)
			g_string_append_printf (cdata->gstr,"%s\t:to (",
						cdata->from ? ")\n" : "");
		g_string_append (cdata->gstr, pair);
		cdata->to = TRUE;
	}
	
	if (mu_msg_contact_type (c) == MU_MSG_CONTACT_TYPE_CC) {
		if (!cdata->cc)
			g_string_append_printf (cdata->gstr,"%s\t:cc (",
						cdata->from||cdata->to ?
						")\n" : "");
		g_string_append (cdata->gstr, pair);
		cdata->cc = TRUE;
	}

	if (mu_msg_contact_type (c) == MU_MSG_CONTACT_TYPE_BCC) {
		if (!cdata->bcc)
			g_string_append_printf
				(cdata->gstr, "%s\t:bcc (",
				 cdata->from||cdata->to||cdata->cc ?
				 ")\n" : "");
		g_string_append (cdata->gstr, pair);
		cdata->bcc = TRUE;
	}

	g_free (pair);
	
	return TRUE;
}


static void
append_sexp_contacts (GString *gstr, MuMsg *msg)
{
	ContactData cdata = { FALSE, FALSE, FALSE, FALSE, gstr};
	
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)each_contact,
				&cdata);
	
	if (cdata.from || cdata.to || cdata.cc || cdata.bcc)
		gstr = g_string_append (gstr, ")\n");
}

struct _FlagData {
	char *flagstr;
	MuMsgFlags msgflags;
};
typedef struct _FlagData FlagData;

static void
each_flag (MuMsgFlags flag, FlagData *fdata)
{
	if (!(flag & fdata->msgflags))
		return;

	if (!fdata->flagstr)
		fdata->flagstr = g_strdup (mu_msg_flag_name(flag));
	else {
		gchar *tmp;
		tmp = g_strconcat (fdata->flagstr, " ",
				   mu_msg_flag_name(flag), NULL);
		g_free (fdata->flagstr);
		fdata->flagstr = tmp;
	}
}

static void
append_sexp_flags (GString *gstr, MuMsg *msg)
{
	FlagData fdata;
	
	fdata.msgflags = mu_msg_get_flags (msg);
	fdata.flagstr  = NULL;
	
	mu_msg_flags_foreach ((MuMsgFlagsForeachFunc)each_flag, &fdata);
	if (fdata.flagstr) 
		g_string_append_printf (gstr, "\t:flags (%s)\n",
					fdata.flagstr);
}


char*
mu_msg_to_sexp (MuMsg *msg, gboolean dbonly)
{
	GString *gstr;
	time_t t;
	
	gstr = g_string_sized_new (dbonly ? 1024 : 8192);	
	g_string_append (gstr, "(\n");

	append_sexp_contacts (gstr, msg);	
	append_sexp_attr (gstr,
			  "subject", mu_msg_get_subject (msg));
	
	t = mu_msg_get_date (msg);
	g_string_append_printf (gstr,
				"\t:date (%u %u 0)\n", (unsigned)(t >> 16),
				(unsigned)(t & 0xffff));
	g_string_append_printf (gstr, "\t:size %u\n",
				(unsigned) mu_msg_get_size (msg));
	
	append_sexp_attr (gstr, "msgid", mu_msg_get_msgid (msg));
	append_sexp_attr (gstr, "path",	   mu_msg_get_path (msg));
	
	
	append_sexp_attr (gstr, "maildir", mu_msg_get_maildir (msg));

	g_string_append_printf (gstr, "\t:priority %s\n",
				mu_msg_prio_name(mu_msg_get_prio(msg)));
	
	append_sexp_flags (gstr, msg);
	
	if (!dbonly) {
		append_sexp_attr (gstr, "body-txt",
				  mu_msg_get_body_text(msg));
		append_sexp_attr (gstr, "body-html",
				  mu_msg_get_body_html(msg));
	}
	
	g_string_append (gstr, ")\n;;eom\n");	

	return g_string_free (gstr, FALSE);
}
 

