/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <errno.h>
#include <xapian.h>

#include "mu-util.h"
#include "mu-msg-fields.h"
#include "mu-msg-doc.h"
#include "mu-str.h"
#include "mu-date.h"

struct _MuMsgDoc {

	_MuMsgDoc (Xapian::Document *doc): _doc (doc) { }
	~_MuMsgDoc () {	delete _doc; }
	const Xapian::Document doc() const { return *_doc; }
private:
	Xapian::Document *_doc;
};


MuMsgDoc*
mu_msg_doc_new (XapianDocument *doc, GError **err)
{
	g_return_val_if_fail (doc, NULL);

	try {
		return new MuMsgDoc ((Xapian::Document*)doc);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, NULL);

	return FALSE;
}

void
mu_msg_doc_destroy (MuMsgDoc *self)
{
	try {
		delete self;

	} MU_XAPIAN_CATCH_BLOCK;
}


gchar*
mu_msg_doc_get_str_field (MuMsgDoc *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), NULL);

	// disable this check:
	//    g_return_val_if_fail (mu_msg_field_is_string(mfid), NULL);
	// because it's useful to get numerical field as strings,
	// for example when sorting (which is much faster if don't
	// have to convert to numbers first, esp. when it's a date
	// time_t)

	try {
		const std::string s (self->doc().get_value(mfid));
		return s.empty() ? NULL : g_strdup (s.c_str());

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


GSList*
mu_msg_doc_get_str_list_field (MuMsgDoc *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), NULL);
	g_return_val_if_fail (mu_msg_field_is_string_list(mfid), NULL);

	try {
		/* return a comma-separated string as a GSList */
		const std::string s (self->doc().get_value(mfid));
		return s.empty() ? NULL : mu_str_to_list(s.c_str(),',',TRUE);

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


gint64
mu_msg_doc_get_num_field (MuMsgDoc *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, -1);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), -1);
	g_return_val_if_fail (mu_msg_field_is_numeric(mfid), -1);

	/* date is a special case, because we store dates as
	 * strings */
	try {
		const std::string s (self->doc().get_value(mfid));
		if (s.empty())
			return 0;
		else if (mfid == MU_MSG_FIELD_ID_DATE) {
			time_t t;
			t = mu_date_str_to_time_t (s.c_str(), FALSE/*utc*/);
			return static_cast<gint64>(t);
		} else {
			return static_cast<gint64>
				(Xapian::sortable_unserialise(s));
		}

	} MU_XAPIAN_CATCH_BLOCK_RETURN(-1);
}
