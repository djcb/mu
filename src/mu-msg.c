/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include <gmime/gmime.h>

#include "mu-msg-priv.h" /* include before mu-msg.h */
#include "mu-msg.h"

#include "mu-util.h"
#include "mu-str.h"

/* note, we do the gmime initialization here rather than in
 * mu-runtime, because this way we don't need mu-runtime for simple
 * cases -- such as our unit tests */
static gboolean _gmime_initialized = FALSE;

static void
gmime_init (void)
{
	g_return_if_fail (!_gmime_initialized);

#ifdef GMIME_ENABLE_RFC2047_WORKAROUNDS
	g_mime_init(GMIME_ENABLE_RFC2047_WORKAROUNDS);
#else
	g_mime_init(0);
#endif /* GMIME_ENABLE_RFC2047_WORKAROUNDS */

	_gmime_initialized = TRUE;
}

static void
gmime_uninit (void)
{
	g_return_if_fail (_gmime_initialized);

	g_mime_shutdown();
	_gmime_initialized = FALSE;
}

MuMsg*
mu_msg_new_from_file (const char *path, const char *mdir, GError **err)
{
	MuMsg *self;
	MuMsgFile *msgfile;
	
	g_return_val_if_fail (path, NULL);
	
	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init ();
		g_atexit (gmime_uninit);
	}

	msgfile = mu_msg_file_new (path, mdir, err);
	if (!msgfile) 
		return NULL;
	
	self = g_slice_new0 (MuMsg);

	self->_file	= msgfile;
	self->_refcount = 1;
	
	return self;
}


static void 
mu_msg_destroy (MuMsg *self)
{
	if (!self)
		return;

	mu_msg_file_destroy (self->_file);
	
	g_slice_free (MuMsg, self);
}


MuMsg*
mu_msg_ref (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);

	++self->_refcount;
	
	return self;
}

void
mu_msg_unref (MuMsg *self)
{
	g_return_if_fail (self);
	g_return_if_fail (self->_refcount >= 1);
	
	if (--self->_refcount == 0) 
		mu_msg_destroy (self);
}


const char*    
mu_msg_get_path  (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_PATH);
}


const char*    
mu_msg_get_subject  (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_SUBJECT);
}

const char*    
mu_msg_get_msgid  (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_MSGID);
}

const char*    
mu_msg_get_maildir (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_MAILDIR);
}


const char*    
mu_msg_get_from (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_FROM);
}


const char*    
mu_msg_get_to (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_TO);
}

const char*    
mu_msg_get_cc (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_CC);
}


const char*    
mu_msg_get_bcc (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_BCC);
}


time_t
mu_msg_get_date (MuMsg *self)
{
	g_return_val_if_fail (self, (time_t)-1);
	return (time_t)mu_msg_file_get_num_field (self->_file,
						  MU_MSG_FIELD_ID_DATE);
}



MuMsgFlags
mu_msg_get_flags (MuMsg *self)
{
	g_return_val_if_fail (self, MU_MSG_FLAG_NONE);
	return (MuMsgFlags)mu_msg_file_get_num_field (self->_file,
						      MU_MSG_FIELD_ID_FLAGS);
}

size_t
mu_msg_get_size (MuMsg *self)
{
	g_return_val_if_fail (self, 0);
	return (size_t)mu_msg_file_get_num_field (self->_file,
						  MU_MSG_FIELD_ID_SIZE);
}


MuMsgPrio
mu_msg_get_prio (MuMsg *self)
{
	g_return_val_if_fail (self, (time_t)-1);
	return (MuMsgPrio)mu_msg_file_get_num_field (self->_file,
						     MU_MSG_FIELD_ID_PRIO);
}


/* const char*      */
/* mu_msg_get_header (MuMsg *msg, const char* header) */
/* { */
/* 	g_return_val_if_fail (self, NULL); */
/* 	g_return_val_if_fail (header, NULL); */

/* 	return g_mime_object_get_header (GMIME_OBJECT(self->_mime_msg),  */
/* 					 header); */
/* } */


time_t
mu_msg_get_timestamp (MuMsg *self)
{
	g_return_val_if_fail (self, (time_t)-1);
	return (MuMsgPrio)mu_msg_file_get_num_field (self->_file,
						     MU_MSG_FIELD_ID_TIMESTAMP);
}


const char*
mu_msg_get_body_html (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_BODY_HTML);
}


const char*
mu_msg_get_body_text (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_BODY_TEXT);
}


const char*
mu_msg_get_summary (MuMsg *self, size_t max_lines)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (max_lines > 0, NULL);
	
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_SUMMARY);
}

const char*
mu_msg_get_references_str (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file,
					  MU_MSG_FIELD_ID_REFS);
}


const char*
mu_msg_get_field_string (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	return mu_msg_file_get_str_field (self->_file, mfid);
}

gint64
mu_msg_get_field_numeric (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, -1);
	return mu_msg_file_get_num_field (self->_file, mfid);
}

