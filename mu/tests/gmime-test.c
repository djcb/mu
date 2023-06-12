/*
** Copyright (C) 2011-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#define _POSIX_C_SOURCE 1

#include <gmime/gmime.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <locale.h>

static gchar*
get_recip(GMimeMessage* msg, GMimeAddressType atype)
{
	char*                recep;
	InternetAddressList* receps;

	receps = g_mime_message_get_addresses(msg, atype);
	recep  = (char*)internet_address_list_to_string(receps, NULL, FALSE);

	if (!recep || !*recep) {
		g_free(recep);
		return NULL;
	}

	return recep;
}

static gchar*
get_refs_str(GMimeMessage* msg)
{
	const gchar*     str;
	GMimeReferences* mime_refs;
	int              i, refs_len;
	gchar*           rv;

	str = g_mime_object_get_header(GMIME_OBJECT(msg), "References");
	if (!str)
		return NULL;

	mime_refs = g_mime_references_parse(NULL, str);
	refs_len  = g_mime_references_length(mime_refs);
	for (rv = NULL, i = 0; i < refs_len; ++i) {
		const char* msgid;
		char *tmp;

		msgid = g_mime_references_get_message_id(mime_refs, i);
		tmp = rv;
		rv    = g_strdup_printf("%s%s%s", rv ? rv : "", rv ? "," : "", msgid);
		g_free(tmp);
	}
	g_mime_references_free(mime_refs);

	return rv;
}

static void
print_date(GMimeMessage* msg)
{
	GDateTime* dt;
	gchar*     buf;

	dt = g_mime_message_get_date(msg);
	if (!dt)
		return;

	dt  = g_date_time_to_local(dt);
	buf = g_date_time_format(dt, "%c");
	g_date_time_unref(dt);

	if (buf) {
		g_print("Date   : %s\n", buf);
		g_free(buf);
	}
}

static void
print_body(GMimeMessage* msg)
{
	GMimeObject*      body;
	GMimeDataWrapper* wrapper;
	GMimeStream*      stream;

	body = g_mime_message_get_body(msg);

	if (GMIME_IS_MULTIPART(body))
		body = g_mime_multipart_get_part(GMIME_MULTIPART(body), 0);
	if (!GMIME_IS_PART(body))
		return;

	wrapper = g_mime_part_get_content(GMIME_PART(body));
	if (!GMIME_IS_DATA_WRAPPER(wrapper))
		return;

	stream = g_mime_data_wrapper_get_stream(wrapper);
	if (!GMIME_IS_STREAM(stream))
		return;

	do {
		char    buf[512];
		ssize_t len;

		len = g_mime_stream_read(stream, buf, sizeof(buf));
		if (len == -1)
			break;

		if (write(fileno(stdout), buf, len) == -1)
			break;

		if (len < (int)sizeof(buf))
			break;

	} while (1);
}

static gboolean
test_message(GMimeMessage* msg)
{
	gchar*       val;
	const gchar* str;

	val = get_recip(msg, GMIME_ADDRESS_TYPE_FROM);
	g_print("From   : %s\n", val ? val : "<none>");
	g_free(val);

	val = get_recip(msg, GMIME_ADDRESS_TYPE_TO);
	g_print("To     : %s\n", val ? val : "<none>");
	g_free(val);

	val = get_recip(msg, GMIME_ADDRESS_TYPE_CC);
	g_print("Cc     : %s\n", val ? val : "<none>");
	g_free(val);

	val = get_recip(msg, GMIME_ADDRESS_TYPE_BCC);
	g_print("Bcc    : %s\n", val ? val : "<none>");
	g_free(val);

	str = g_mime_message_get_subject(msg);
	g_print("Subject: %s\n", str ? str : "<none>");

	print_date(msg);

	str = g_mime_message_get_message_id(msg);
	g_print("Msg-id : %s\n", str ? str : "<none>");

	{
		gchar* refsstr;
		refsstr = get_refs_str(msg);
		g_print("Refs   : %s\n", refsstr ? refsstr : "<none>");
		g_free(refsstr);
	}

	print_body(msg);

	return TRUE;
}

static gboolean
test_stream(GMimeStream* stream)
{
	GMimeParser*  parser;
	GMimeMessage* msg;
	gboolean      rv;

	parser = NULL;
	msg    = NULL;

	parser = g_mime_parser_new_with_stream(stream);
	if (!parser) {
		g_warning("failed to create parser");
		rv = FALSE;
		goto leave;
	}

	msg = g_mime_parser_construct_message(parser, NULL);
	if (!msg) {
		g_warning("failed to construct message");
		rv = FALSE;
		goto leave;
	}

	rv = test_message(msg);

leave:
	if (parser)
		g_object_unref(parser);

	if (msg)
		g_object_unref(msg);

	return rv;
}

static gboolean
test_file(const char* path)
{
	FILE*        file;
	GMimeStream* stream;
	gboolean     rv;

	stream = NULL;
	file   = NULL;

	file = fopen(path, "r");
	if (!file) {
		g_warning("cannot open file '%s': %s", path, g_strerror(errno));
		rv = FALSE;
		goto leave;
	}

	stream = g_mime_stream_file_new(file);
	if (!stream) {
		g_warning("cannot open stream for '%s'", path);
		rv = FALSE;
		goto leave;
	}

	rv = test_stream(stream);
	g_object_unref(stream);
	return rv;

leave:
	if (file)
		fclose(file);

	return rv;
}

int
main(int argc, char* argv[])
{
	gboolean rv;

	if (argc != 2) {
		g_printerr("usage: %s <msg-file>\n", argv[0]);
		return 1;
	}

	setlocale(LC_ALL, "");

	g_mime_init();

	rv = test_file(argv[1]);

	g_mime_shutdown();

	return rv ? 0 : 1;
}
