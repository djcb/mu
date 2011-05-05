/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-contacts.h"
#include "mu-util.h"
#include "mu-str.h"

#define MU_CONTACTS_NAME_KEY		"name"
#define MU_CONTACTS_TIMESTAMP_KEY	"timestamp"

struct _ContactInfo {
	gchar *_name;
	time_t _tstamp;
};
typedef struct _ContactInfo ContactInfo;

static void contact_info_destroy (ContactInfo *cinfo);
static ContactInfo *contact_info_new (char *name, time_t tstamp);

struct _MuContacts {
        GKeyFile      *_ccache;
	gchar         *_ccachefile;
	GHashTable    *_hash;
	gboolean       _dirty;
};


static gboolean
unserialize_cache (MuContacts *self)
{
	GError *err;
	gchar **groups;
	gsize i, len;

	/* if there is no file yet, don't try to open; otherwise return FALSE */
	if (access(self->_ccachefile, F_OK) != 0) 
		return errno == ENOENT ? TRUE : FALSE;
				
	err = NULL;
	/* try to unserialize the cache */
	if (!g_key_file_load_from_file (self->_ccache, self->_ccachefile,
					G_KEY_FILE_KEEP_COMMENTS, &err)) {
		/* not necessarily an error, so not bailing out */
		g_warning ("could not load keyfile %s: %s",
			   self->_ccachefile, err->message);
		g_error_free (err);
		return FALSE;
	}

	groups = g_key_file_get_groups (self->_ccache, &len);
	for (i = 0; i  != len; ++i) {
		ContactInfo *cinfo;
		cinfo = contact_info_new (/* note, contact_info_new will *own* the string param,
					   * and take care of freeing it */
			g_key_file_get_string (self->_ccache, groups[i],
					       MU_CONTACTS_NAME_KEY, NULL),
			(time_t)g_key_file_get_integer (self->_ccache, groups[i],
							MU_CONTACTS_TIMESTAMP_KEY, NULL));
		/* note, we're using the groups[i], so don't free with g_strfreev */
		g_hash_table_insert (self->_hash, groups[i], cinfo);
	}

	g_free (groups);
	return TRUE;
}


MuContacts*
mu_contacts_new (const gchar *ccachefile)
{
	MuContacts *contacts;
	
	g_return_val_if_fail (ccachefile, NULL);
	contacts = g_new0 (MuContacts, 1);
	
	contacts->_ccachefile = g_strdup (ccachefile);
	contacts->_ccache     = g_key_file_new ();
	if (!contacts->_ccache) {
		mu_contacts_destroy (contacts);
		return NULL;
	}
	
	contacts->_hash = g_hash_table_new_full
		(g_str_hash, g_str_equal,
		 g_free, (GDestroyNotify)contact_info_destroy);

	unserialize_cache (contacts);
	MU_WRITE_LOG("unserialized contacts cache %s", ccachefile);

	contacts->_dirty = FALSE;
	return contacts;
}


gboolean
mu_contacts_add (MuContacts *self, const char* name, const char *email,
		 time_t tstamp)
{
	ContactInfo *cinfo;
	
	g_return_val_if_fail (self, FALSE);
	g_return_val_if_fail (email, FALSE);

	/* add the info, if either there is no info for this email
	 * yet, *OR* the new one is more recent and does not have an
	 * empty name */	
	cinfo = (ContactInfo*) g_hash_table_lookup (self->_hash, email);
	if (!cinfo ||
	    (cinfo->_tstamp < tstamp && !mu_str_is_empty(name))) {	
		ContactInfo *ci; /* note ci will take care of freeing the first param */
		ci = contact_info_new (name ? g_strdup(name) : NULL, tstamp);
		g_hash_table_insert (self->_hash, g_strdup(email), ci);
		return self->_dirty = TRUE;
	}
	
	return FALSE;
}

struct _EachContactData {
	MuContactsForeachFunc	 _func;
	gpointer		 _user_data;
	GRegex			*_rx;
	size_t                   _num;
};
typedef struct _EachContactData	 EachContactData;

static void /* email will never be NULL, but ci->_name may be */
each_contact (const char* email, ContactInfo *ci, EachContactData *ecdata)
{
	/* ignore this contact if we have a regexp, and it matches
	* neither email nor name (if we have a name) */
	while (ecdata->_rx) { /* note, only once */
		if (g_regex_match (ecdata->_rx, email, 0, NULL))
			break; /* email matches? continue! */
		if (!ci->_name)
			return; /* email did not match, no name? ignore this one */
		if (g_regex_match (ecdata->_rx,ci->_name, 0, NULL))
			break; /* name matches? continue! */
		return; /* nothing matched, ignore this one */
	}
	
	ecdata->_func (email, ci->_name, ci->_tstamp, ecdata->_user_data);
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
	
	g_hash_table_foreach (self->_hash, (GHFunc) each_contact, &ecdata);

	if (ecdata._rx)
		g_regex_unref (ecdata._rx);
	
	if (num)
		*num = ecdata._num;
	
	return TRUE;
}





static void
each_keyval (const char *email, ContactInfo *cinfo, MuContacts *self)
{
	if (cinfo->_name)
		g_key_file_set_string (self->_ccache, email, "name",
				       cinfo->_name);

	g_key_file_set_integer (self->_ccache, email, "timestamp",
				(int)cinfo->_tstamp);
}

static gboolean
serialize_cache (MuContacts *self)
{
	gchar *data;
	gsize len;
	gboolean rv;

	g_hash_table_foreach (self->_hash, (GHFunc) each_keyval, self);

	/* Note: err arg is unused */
	data = g_key_file_to_data (self->_ccache, &len, NULL);
	if (len) {
		GError *err;
		err = NULL;
		rv = g_file_set_contents (self->_ccachefile, data, len, &err);
		if (!rv) {
			g_warning ("failed to serialize cache to %s: %s",
				   self->_ccachefile, err->message);
			g_error_free (err);
		}
		g_free (data);
	}

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
			     self->_ccachefile);
	}

	if (self->_ccache)
		g_key_file_free (self->_ccache);

	g_free (self->_ccachefile);
	
	if (self->_hash)
		g_hash_table_destroy (self->_hash);
	
	g_free (self);	
}


/* note, we will *own* the name we get */
static ContactInfo *
contact_info_new (char *name, time_t tstamp)
{
	ContactInfo *cinfo;
	
	cinfo	       = g_slice_new (ContactInfo);
	/* removing leading, trailing whitespace from names */
	cinfo->_name   = name ? g_strstrip(name) : NULL;
	cinfo->_tstamp = tstamp;

	return cinfo;
}

static void
contact_info_destroy (ContactInfo *cinfo)
{
	if (!cinfo)
		return;
	
	g_free (cinfo->_name);
	g_slice_free (ContactInfo, cinfo);
}
	
