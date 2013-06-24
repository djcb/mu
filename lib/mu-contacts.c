/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "mu-contacts.h"
#include "mu-util.h"
#include "mu-str.h"

#define EMAIL_KEY	"email"
#define NAME_KEY	"name"
#define TSTAMP_KEY	"tstamp"
#define PERSONAL_KEY	"personal"
#define FREQ_KEY	"frequency"

/* note: 'personal' here means a mail where my e-mail addresses is explicitly
 * in one of the address fields, ie., it's not some mailing list message */
struct _ContactInfo {
	gchar          *_name, *_email;
	gboolean	_personal;
	time_t		_tstamp;
	unsigned	_freq;
};
typedef struct _ContactInfo ContactInfo;

static void contact_info_destroy (ContactInfo *cinfo);
static ContactInfo *contact_info_new (char *email, char *name,
				      gboolean personal, time_t tstamp, unsigned freq);

struct _MuContacts {
        GKeyFile	*_ccache;
	gchar		*_path;

	GHashTable	*_hash;
	gboolean	 _dirty;
};

static GKeyFile*
load_key_file (const char *path)
{
	GError		*err;
	GKeyFile	*keyfile;
	gboolean	 file_exists;

	err	    = NULL;

	/* of course this is racy, but it's only for giving more
	 * meaningful errors to users */
	file_exists = TRUE;
	if (access(path, F_OK) != 0) {
		if (errno != ENOENT) {
			g_warning ("cannot open %s: %s", path, strerror(errno));
			return NULL;
		}
		file_exists = FALSE;
	}

	err = NULL;
	keyfile = g_key_file_new ();

	if (file_exists && !g_key_file_load_from_file
	    (keyfile, path, G_KEY_FILE_KEEP_COMMENTS, &err)) {
		g_warning ("could not load keyfile %s: %s", path, err->message);
		g_error_free (err);
		g_key_file_free (keyfile);
		return NULL;
	}
	return keyfile;
}


static gboolean
get_values (GKeyFile *kfile, const gchar *group,
	    gchar **email, gchar **name, gboolean *personal, size_t *tstamp,
	    unsigned *freq)
{
	GError *err;
	err = NULL;

	do {
		int i;

		*email = g_key_file_get_value (kfile, group, EMAIL_KEY, &err);
		if (!*email)
			break;

		*tstamp = (time_t)g_key_file_get_integer (kfile, group,
							  TSTAMP_KEY, &err);
		if (err)
			break;
		*personal = g_key_file_get_boolean (kfile, group,
						    PERSONAL_KEY, NULL);
		*name = g_key_file_get_value (kfile, group, NAME_KEY, NULL);

		i = g_key_file_get_integer (kfile, group, FREQ_KEY, NULL);
		*freq = (unsigned)(i >= 0) ? i : 1;

		return TRUE;

	} while (0);

	g_warning ("error getting value for %s: %s",
		   group, err->message ? err->message: "error");
	g_clear_error (&err);

	return FALSE;
}


static gboolean
deserialize_cache (MuContacts *self)
{
	gchar **groups;
	gsize i, len;

	groups = g_key_file_get_groups (self->_ccache, &len);
	for (i = 0; i != len; ++i) {
		ContactInfo *cinfo;
		char *name, *email;
		size_t tstamp;
		gboolean personal;
		unsigned freq;
		if (!get_values (self->_ccache, groups[i],
				 &email, &name, &personal, &tstamp, &freq))
			continue; /* ignore this one... */

		cinfo = contact_info_new (email, name, personal, tstamp, freq);

		/* note, we're using the groups[i], so don't free with g_strfreev */
		g_hash_table_insert (self->_hash, groups[i],
				     cinfo);
	}

	g_free (groups);
	return TRUE;
}

static gboolean
set_comment (GKeyFile *kfile)
{
	GError *err;
	const char *comment =
		" automatically generated -- do not edit";

	err = NULL;
	if (!g_key_file_set_comment (kfile, NULL, NULL, comment, &err)) {
		g_warning ("could not write comment to keyfile: %s",
			   err->message);
		g_error_free (err);
		return FALSE;
	}

	return TRUE;
}


MuContacts*
mu_contacts_new (const gchar *path)
{
	MuContacts *self;

	g_return_val_if_fail (path, NULL);
	self = g_new0 (MuContacts, 1);

	self->_path = g_strdup (path);
	self->_hash = g_hash_table_new_full
		(g_str_hash, g_str_equal, g_free,
		 (GDestroyNotify)contact_info_destroy);

	self->_ccache  = load_key_file (path);
	if (!self->_ccache || !set_comment (self->_ccache)) {
		mu_contacts_destroy (self);
		return NULL;
	}
	deserialize_cache (self);
	MU_WRITE_LOG("deserialized contacts from cache %s",
		     path);

	self->_dirty = FALSE;
	return self;
}


void
mu_contacts_clear (MuContacts *self)
{
	g_return_if_fail (self);

	if (self->_ccache)
		g_key_file_free (self->_ccache);

	g_hash_table_remove_all (self->_hash);

	self->_ccache = g_key_file_new ();
	self->_dirty = FALSE;
}


/*
 * we use the e-mail address to create a key in the GKeyFile, but we
 * have to mutilate a bit so that it's (a) *cough* practically-unique
 * and (b) valid as a GKeyFile group name (ie., valid utf8, no control
 * chars, no '[' or ']')
 */
static const char*
encode_email_address (const char *addr)
{
	static char enc[254 + 1]; /* max size for an e-mail addr */
	char *cur;

	if (!addr)
		return FALSE;

	/* make sure chars are with {' ' .. '~'}, and not '[' ']' */
	for (cur = strncpy(enc, addr, sizeof(enc)); *cur != '\0'; ++cur)
		if (!isalnum(*cur)) {
			*cur = 'A' +  (*cur % ('Z' - 'A'));
		} else
			*cur = tolower(*cur);

	return enc;
}


/* downcase the domain-part of the email address, but only if it
 * consists of ascii (to prevent screwing up idna addresses)
 */
char*
downcase_domain_maybe (const char *addr)
{
	char *addr_conv, *at, *cur;

	addr_conv = g_strdup (addr);

	if (!(at = strchr (addr_conv, '@'))) {	/*huh?*/
		g_free (addr_conv);
		return NULL;
	}

	for (cur = at + 1; *cur; ++cur) {
		if (isascii(*cur))
			*cur = g_ascii_tolower (*cur);
		else { /* non-ascii; return the unchanged original */
			g_free (addr_conv);
			return g_strdup (addr);
		}

	}

	return addr_conv;
}

static void
clear_str (char* str)
{
	if (str) {
		mu_str_remove_ctrl_in_place (str);
		g_strstrip (str);
	}
}




gboolean
mu_contacts_add (MuContacts *self, const char *addr, const char *name,
		 gboolean personal, time_t tstamp)
{
	ContactInfo *cinfo;
	const char *group;

	g_return_val_if_fail (self, FALSE);
	g_return_val_if_fail (addr, FALSE);

	group	= encode_email_address (addr);

	cinfo = (ContactInfo*) g_hash_table_lookup (self->_hash, group);
	if (!cinfo) {
		char *addr_dc;
		if (!(addr_dc = downcase_domain_maybe (addr)))
			return FALSE;
		cinfo = contact_info_new (addr_dc,
					  name ? g_strdup(name) : NULL, personal,
					  tstamp, 1);
		g_hash_table_insert (self->_hash, g_strdup(group), cinfo);
	} else {
		if (cinfo->_tstamp < tstamp) {
			if (!mu_str_is_empty(name)) {
				/* update the name to the last one used, unless it's
				 * empty*/
				g_free (cinfo->_name);
				cinfo->_name = g_strdup (name);
				if (cinfo->_name)
					mu_str_remove_ctrl_in_place (cinfo->_name);
			}
			cinfo->_tstamp = tstamp;
		}
		++cinfo->_freq;
	}


	self->_dirty = TRUE;

	return TRUE;
}

struct _EachContactData {
	MuContactsForeachFunc	 _func;
	gpointer		 _user_data;
	GRegex			*_rx;
	size_t                   _num;
};
typedef struct _EachContactData	 EachContactData;

static void /* email will never be NULL, but ci->_name may be */
each_contact (const char *group, ContactInfo *ci, EachContactData *ecdata)
{
	if (!ci->_email)
		g_warning ("missing email: %u", (unsigned)ci->_tstamp);

	/* ignore this contact if we have a regexp, and it matches
	 * neither email nor name (if we have a name) */
	while (ecdata->_rx) { /* note, only once */
		if (g_regex_match (ecdata->_rx, ci->_email, 0, NULL))
			break; /* email matches? continue! */
		if (!ci->_name)
			return; /* email did not match, no name? ignore this one */
		if (g_regex_match (ecdata->_rx, ci->_name, 0, NULL))
			break; /* name matches? continue! */
		return; /* nothing matched, ignore this one */
	}

	ecdata->_func (ci->_email, ci->_name, ci->_personal,
		       ci->_tstamp, ci->_freq, ecdata->_user_data);

	++ecdata->_num;
}

gboolean
mu_contacts_foreach (MuContacts *self, MuContactsForeachFunc func,
		     gpointer user_data, const char *pattern, size_t *num)
{
	EachContactData ecdata;

	g_return_val_if_fail (self, FALSE);
	g_return_val_if_fail (func, FALSE);

	if (pattern) {
		GError *err;
		err = NULL;
		ecdata._rx = g_regex_new
			(pattern, G_REGEX_CASELESS|G_REGEX_OPTIMIZE,
			 0, &err);
		if (!ecdata._rx) {
			g_warning ("error in regexp '%s': %s",
				   pattern, err->message);
			g_error_free (err);
			return FALSE;
		}
	} else
		ecdata._rx = NULL;

	ecdata._func	  = func;
	ecdata._user_data = user_data;
	ecdata._num       = 0;

	g_hash_table_foreach (self->_hash,
			      (GHFunc)each_contact,
			      &ecdata);

	if (ecdata._rx)
		g_regex_unref (ecdata._rx);

	if (num)
		*num = ecdata._num;

	return TRUE;
}

static void
each_keyval (const char *group, ContactInfo *cinfo, MuContacts *self)
{
	/* use set value so the string do not necessarily have to be
	 * valid utf-8 */
	if (cinfo->_name)
		g_key_file_set_value (self->_ccache, group, NAME_KEY,
				       cinfo->_name);

	g_key_file_set_value (self->_ccache, group, EMAIL_KEY,
			       cinfo->_email);
	g_key_file_set_boolean (self->_ccache, group, PERSONAL_KEY,
				cinfo->_personal);
	g_key_file_set_integer (self->_ccache, group, TSTAMP_KEY,
				(int)cinfo->_tstamp);
	g_key_file_set_integer (self->_ccache, group, FREQ_KEY,
				(int)cinfo->_freq);
}

static gboolean
serialize_cache (MuContacts *self)
{
	gchar *data;
	gsize len;
	gboolean rv;

	g_hash_table_foreach (self->_hash, (GHFunc)each_keyval, self);

	/* Note: err arg is unused */
	data = g_key_file_to_data (self->_ccache, &len, NULL);
	if (len) {
		GError *err;
		err = NULL;
		rv = g_file_set_contents (self->_path, data, len, &err);
		if (!rv) {
			g_warning ("failed to serialize cache to %s: %s",
				   self->_path, err->message);
			g_error_free (err);
		}
		g_free (data);
	} else
		rv = TRUE;

	return rv;
}

void
mu_contacts_destroy (MuContacts *self)
{
	if (!self)
		return;

	if (self->_ccache && self->_dirty) {
		serialize_cache (self);
		MU_WRITE_LOG("serialized contacts cache %s",
			     self->_path);
	}

	if (self->_ccache)
		g_key_file_free (self->_ccache);

	g_free (self->_path);

	if (self->_hash)
		g_hash_table_destroy (self->_hash);

	g_free (self);
}


/* note, we will *own* the name, email we get, and we'll free them in
 * the end... */
static ContactInfo *
contact_info_new (char *email, char *name, gboolean personal, time_t tstamp,
		  unsigned freq)
{
	ContactInfo *cinfo;

	/* email should not be NULL, name can */
	g_return_val_if_fail (email, NULL);

	cinfo = g_slice_new (ContactInfo);

	/* we need to clear the strings from control chars because
	 * they could screw up the keyfile */
	clear_str (email);
	clear_str (name);

	cinfo->_email    = email;
	cinfo->_name     = name;
	cinfo->_personal = personal;
	cinfo->_tstamp   = tstamp;
	cinfo->_freq     = freq;

	return cinfo;
}

static void
contact_info_destroy (ContactInfo *cinfo)
{
	if (!cinfo)
		return;

	g_free (cinfo->_email);
	g_free (cinfo->_name);

	g_slice_free (ContactInfo, cinfo);
}


size_t
mu_contacts_count (MuContacts *self)
{
	g_return_val_if_fail (self, 0);

	return g_hash_table_size (self->_hash);
}
