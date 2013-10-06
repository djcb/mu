/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@cthulhu>
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

/* gmime-test; compile with:
       gcc -o gmime-test gmime-test.c -Wall -O0 -ggdb3 \
           `pkg-config --cflags --libs gmime-2.4`
 */

#include <gmime/gmime.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <locale.h>

static gchar*
get_recip (GMimeMessage *msg, GMimeRecipientType rtype)
{
	char *recep;
        InternetAddressList *receps;

        receps = g_mime_message_get_recipients (msg, rtype);
        recep = (char*)internet_address_list_to_string (receps, FALSE);

        if (!recep || !*recep) {
                g_free (recep);
                return NULL;
        }

        return recep;
}

static gchar*
get_refs_str (GMimeMessage *msg)
{
	const gchar *str;
	const GMimeReferences *cur;
	GMimeReferences *mime_refs;
	gchar *rv;

	str = g_mime_object_get_header (GMIME_OBJECT(msg), "References");
	if (!str)
		return NULL;

	mime_refs = g_mime_references_decode (str);
	for (rv = NULL, cur = mime_refs; cur;
	     cur = g_mime_references_get_next(cur)) {

		const char* msgid;
		msgid = g_mime_references_get_message_id (cur);
		rv = g_strdup_printf ("%s%s%s",
				      rv ? rv : "",
				      rv ? "," : "",
				      msgid);
	}
	g_mime_references_free (mime_refs);

	return rv;
}

static void
print_date (GMimeMessage *msg)
{
	time_t		 t;
	int		 tz;
	char		 buf[64];
	size_t		 len;
	struct  tm	*t_m;


	g_mime_message_get_date (msg, &t, &tz);
	t_m = localtime (&t);

	len = strftime (buf, sizeof(buf) - 1, "%c", t_m);

	if (len > 0)
		g_print ("Date   : %s (%s%04d)\n",
			 buf,tz < 0 ? "-" : "+", tz);
}

static gboolean
test_message (GMimeMessage *msg)
{
	gchar *val;
	const gchar *str;

	g_print ("From   : %s\n", g_mime_message_get_sender (msg));

	val = get_recip (msg, GMIME_RECIPIENT_TYPE_TO);
	g_print ("To     : %s\n", val ? val : "<none>" );
	g_free (val);

	val = get_recip (msg, GMIME_RECIPIENT_TYPE_CC);
	g_print ("Cc     : %s\n", val ? val : "<none>" );
	g_free (val);

	val = get_recip (msg, GMIME_RECIPIENT_TYPE_BCC);
	g_print ("Bcc    : %s\n", val ? val : "<none>" );
	g_free (val);

	str = g_mime_message_get_subject (msg);
	g_print ("Subject: %s\n", str ? str : "<none>");

	print_date (msg);


	str = g_mime_message_get_message_id (msg);
	g_print ("Msg-id : %s\n", str ? str : "<none>");

	{
		gchar *refsstr;
		refsstr = get_refs_str (msg);
		g_print ("Refs   : %s\n", refsstr ? refsstr : "<none>");
		g_free (refsstr);
	}


	return TRUE;
}



static gboolean
test_stream (GMimeStream *stream)
{
	GMimeParser *parser;
	GMimeMessage *msg;
	gboolean rv;

	parser = NULL;
	msg    = NULL;

	parser = g_mime_parser_new_with_stream (stream);
	if (!parser) {
		g_warning ("failed to create parser");
		rv = FALSE;
		goto leave;
	}

	msg = g_mime_parser_construct_message (parser);
	if (!msg) {
		g_warning ("failed to construct message");
		rv = FALSE;
		goto leave;
	}

	rv = test_message (msg);

leave:
	if (parser)
		g_object_unref (parser);
	else
		g_object_unref (stream);

	if (msg)
		g_object_unref (msg);

	return rv;
}


static gboolean
test_file (const char *path)
{
	FILE *file;
	GMimeStream *stream;
	gboolean rv;

	stream = NULL;
	file   = NULL;

	file = fopen (path, "r");
	if (!file) {
		g_warning ("cannot open file '%s': %s", path,
			   strerror(errno));
		rv = FALSE;
		goto leave;
	}

	stream = g_mime_stream_file_new (file);
	if (!stream) {
		g_warning ("cannot open stream for '%s'", path);
		rv = FALSE;
		goto leave;
	}

	rv = test_stream (stream);  /* test-stream will unref it */

leave:
	if (file)
		fclose (file);

	return rv;
}


int
main (int argc, char *argv[])
{
	gboolean rv;

	if (argc != 2) {
		g_printerr ("usage: %s <msg-file>\n", argv[0]);
		return 1;
	}

	setlocale (LC_ALL, "");

	g_mime_init(GMIME_ENABLE_RFC2047_WORKAROUNDS);

	rv = test_file (argv[1]);

	g_mime_shutdown ();

	return rv ? 0 : 1;
}
