/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-message.hh"
#include "mu-msg-doc.hh"

#include "utils/mu-util.h"
#include "utils/mu-str.h"
#include "utils/mu-utils.hh"
#include "utils/mu-xapian-utils.hh"

using namespace Mu;

struct Mu::MuMsgDoc {
	MuMsgDoc(Xapian::Document* doc) : _doc(doc) {}
	~MuMsgDoc() { delete _doc; }
	const Xapian::Document doc() const { return *_doc; }

private:
	Xapian::Document* _doc;
};

MuMsgDoc*
Mu::mu_msg_doc_new(XapianDocument* doc, GError** err)
{
	g_return_val_if_fail(doc, NULL);
	MuMsgDoc* mdoc =
	    xapian_try([&] { return new MuMsgDoc((Xapian::Document*)doc); }, (MuMsgDoc*)nullptr);

	if (!mdoc)
		mu_util_g_set_error(err, MU_ERROR_INTERNAL, "failed to create message doc");
	return mdoc;
}

void
Mu::mu_msg_doc_destroy(MuMsgDoc* self)
{
	xapian_try([&] { delete self; });
}

gchar*
Mu::mu_msg_doc_get_str_field(MuMsgDoc* self, Field::Id field_id)
{
	g_return_val_if_fail(self, NULL);

	// disable this check:
	//    g_return_val_if_fail (mu_msg_field_is_string(field_id), NULL);
	// because it's useful to get numerical field as strings,
	// for example when sorting (which is much faster if don't
	// have to convert to numbers first, esp. when it's a date
	// time_t)

	return xapian_try(
	    [&] {
		    const auto value_no{field_from_id(field_id).value_no()};
		    const std::string s(self->doc().get_value(value_no));
		    return s.empty() ? NULL : g_strdup(s.c_str());
	    },
	    (gchar*)nullptr);
}

GSList*
Mu::mu_msg_doc_get_str_list_field(MuMsgDoc* self, Field::Id field_id)
{
	g_return_val_if_fail(self, NULL);
	g_return_val_if_fail(field_from_id(field_id).type == Field::Type::StringList, NULL);

	return xapian_try(
	    [&] {
		    /* return a comma-separated string as a GSList */
		    const auto value_no{field_from_id(field_id).value_no()};
		    const std::string s(self->doc().get_value(value_no));
		    return s.empty() ? NULL : mu_str_to_list(s.c_str(), ',', TRUE);
	    },
	    (GSList*)nullptr);
}

gint64
Mu::mu_msg_doc_get_num_field(MuMsgDoc* self, Field::Id field_id)
{
	g_return_val_if_fail(self, -1);
	g_return_val_if_fail(field_from_id(field_id).is_numerical(), -1);

	return xapian_try(
	    [&] {
		    const auto value_no{field_from_id(field_id).value_no()};
		    const std::string s(self->doc().get_value(value_no));
		    if (s.empty())
			    return (gint64)0;
		    else if (field_id == Field::Id::Date || field_id == Field::Id::Size)
			    return static_cast<gint64>(strtol(s.c_str(), NULL, 10));
		    else {
			    return static_cast<gint64>(Xapian::sortable_unserialise(s));
		    }
	    },
	    (gint64)-1);
}
