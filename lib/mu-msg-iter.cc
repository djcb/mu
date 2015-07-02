/* -*- mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
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
#include <unistd.h>

#include <iostream>

#include <string.h>
#include <errno.h>
#include <algorithm>
#include <xapian.h>

#include <string>
#include <set>
#include <map>

#include "mu-util.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"
#include "mu-threader.h"


struct ltstr {
	bool operator () (const std::string &s1,
			  const std::string &s2) const {
		return g_strcmp0 (s1.c_str(), s2.c_str()) < 0;
	}
};
typedef std::map <std::string, unsigned, ltstr> msgid_docid_map;

class ThreadKeyMaker: public Xapian::KeyMaker {
public:
	ThreadKeyMaker (GHashTable *threadinfo): _threadinfo(threadinfo) {}
	virtual std::string operator()(const Xapian::Document &doc) const  {
		MuMsgIterThreadInfo *ti;
		ti = (MuMsgIterThreadInfo*)g_hash_table_lookup
			(_threadinfo,
			 GUINT_TO_POINTER(doc.get_docid()));
		return std::string (ti && ti->threadpath ? ti->threadpath : "");
	}
private:
	GHashTable *_threadinfo;
};

struct _MuMsgIter {
public:
	_MuMsgIter (Xapian::Enquire &enq, size_t maxnum,
		    MuMsgFieldId sortfield, MuMsgIterFlags flags):
		_enq(enq), _thread_hash (0), _msg(0), _flags(flags),
		_skip_unreadable(flags & MU_MSG_ITER_FLAG_SKIP_UNREADABLE),
		_skip_dups (flags & MU_MSG_ITER_FLAG_SKIP_DUPS) {

		bool descending       = (flags & MU_MSG_ITER_FLAG_DESCENDING);
		bool threads          = (flags & MU_MSG_ITER_FLAG_THREADS);

		// first, we get _all_ matches (G_MAXINT), based the threads
		// on that, then return <maxint> of those
		_matches         = _enq.get_mset (0, G_MAXINT);

		if (_matches.empty())
			return;

		if (threads) {
			_matches.fetch();
			_cursor = _matches.begin();
			// NOTE: temporarily turn-off skipping duplicates, since we
			// need threadinfo for *all*
			_skip_dups = false;
			_thread_hash = mu_threader_calculate
				(this, _matches.size(), sortfield, descending);
			_skip_dups = (flags & MU_MSG_ITER_FLAG_SKIP_DUPS);
			ThreadKeyMaker	keymaker(_thread_hash);
			enq.set_sort_by_key (&keymaker, false);
			_matches   = _enq.get_mset (0, maxnum);

		} else if (sortfield != MU_MSG_FIELD_ID_NONE) {
			enq.set_sort_by_value ((Xapian::valueno)sortfield,
					       descending);
			_matches   = _enq.get_mset (0, maxnum);
			_cursor	   = _matches.begin();
		}
		_cursor	= _matches.begin();
	}

	~_MuMsgIter () {
		if (_thread_hash)
			g_hash_table_destroy (_thread_hash);

		set_msg (NULL);
	}

	const Xapian::Enquire& enquire() const { return _enq; }
	Xapian::MSet& matches() { return _matches; }

	Xapian::MSet::const_iterator cursor () const { return _cursor; }
	void set_cursor (Xapian::MSetIterator cur) { _cursor = cur; }
	void cursor_next () { ++_cursor; }

	GHashTable *thread_hash () { return _thread_hash; }

	MuMsg *msg() const { return _msg; }
	MuMsg *set_msg (MuMsg *msg) {
		if (_msg)
			mu_msg_unref (_msg);
		return _msg = msg;
	}

	MuMsgIterFlags flags() const { return _flags; }

	const std::string msgid () const {
		const Xapian::Document doc (cursor().get_document());
		return doc.get_value(MU_MSG_FIELD_ID_MSGID);
	}

	unsigned docid () const {
		const Xapian::Document doc (cursor().get_document());
		return doc.get_docid();
	}


	bool looks_like_dup () const {
		try {
			const Xapian::Document doc (cursor().get_document());
			// is this message in the preferred map? if
			// so, it's not a duplicate, otherwise, it
			// isn't
			msgid_docid_map::const_iterator pref_iter (_preferred_map.find (msgid()));
			if (pref_iter != _preferred_map.end()) {
				//std::cerr << "in the set!" << std::endl;
				if ((*pref_iter).second == docid())
					return false; // in the set: not a dup!
				else
					return true;
			}

			// otherwise, simply check if we've already seen this message-id,
			// and, if so, it's considered a dup
			if (_msg_uid_set.find (msgid()) != _msg_uid_set.end()) {
				return true;
			} else {
				_msg_uid_set.insert (msgid());
				return false;
			}
		} catch (...) {
			return true;
		}
	}

	static void each_preferred (const char *msgid, gpointer docidp,
				    msgid_docid_map *preferred_map) {
		(*preferred_map)[msgid] = GPOINTER_TO_SIZE(docidp);
	}

	void set_preferred_map (GHashTable *preferred_hash) {
		if (!preferred_hash)
			_preferred_map.clear();
		else
			g_hash_table_foreach (preferred_hash,
					      (GHFunc)each_preferred, &_preferred_map);
	}

	bool skip_dups ()       const { return _skip_dups; }
	bool skip_unreadable () const { return _skip_unreadable; }

private:
	const Xapian::Enquire		_enq;
	Xapian::MSet			_matches;
	Xapian::MSet::const_iterator	_cursor;

	GHashTable      *_thread_hash;
	MuMsg		*_msg;

	MuMsgIterFlags _flags;

	mutable std::set <std::string, ltstr> _msg_uid_set;
	bool _skip_unreadable;

	// the 'preferred map' (msgid->docid) is used when checking
	// for duplicates; if a message is in the preferred map, it
	// will not be excluded (but other messages with the same
	// msgid will)
	msgid_docid_map _preferred_map;
	bool _skip_dups;
};


static gboolean
is_msg_file_readable (MuMsgIter *iter)
{
	gboolean readable;
	std::string path
		(iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_PATH));

	if (path.empty())
		return FALSE;

	readable = (access (path.c_str(), R_OK) == 0) ? TRUE : FALSE;
	return readable;
}



MuMsgIter *
mu_msg_iter_new (XapianEnquire *enq, size_t maxnum,
		 MuMsgFieldId sortfield, MuMsgIterFlags flags,
		 GError **err)
{
	g_return_val_if_fail (enq, NULL);
	/* sortfield should be set to .._NONE when we're not threading */
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfield) ||
			      sortfield == MU_MSG_FIELD_ID_NONE,
			      FALSE);
	try {
		MuMsgIter *iter (new MuMsgIter ((Xapian::Enquire&)*enq,
						maxnum,
						sortfield,
						flags));
		// note: we check if it's a dup even for the first message,
		// since we need its uid in the set for checking later messages
		if ((iter->skip_unreadable() && !is_msg_file_readable (iter)) ||
		    (iter->skip_dups() && iter->looks_like_dup ()))
			mu_msg_iter_next (iter); /* skip! */

		return iter;

	} catch (const Xapian::DatabaseModifiedError &dbmex) {
		mu_util_g_set_error (err, MU_ERROR_XAPIAN_MODIFIED,
				     "database was modified; please reopen");
		return 0;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}

void
mu_msg_iter_destroy (MuMsgIter *iter)
{
	try { delete iter; } MU_XAPIAN_CATCH_BLOCK;
}



void
mu_msg_iter_set_preferred (MuMsgIter *iter, GHashTable *preferred_hash)
{
	g_return_if_fail (iter);
	iter->set_preferred_map (preferred_hash);
}



MuMsg*
mu_msg_iter_get_msg_floating (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);

	try {
		MuMsg *msg;
		GError *err;
		Xapian::Document *docp;

		docp = new Xapian::Document(iter->cursor().get_document());

		err = NULL;
		msg = iter->set_msg (mu_msg_new_from_doc((XapianDocument*)docp,
							 &err));
		if (!msg)
			MU_HANDLE_G_ERROR(err);

		return msg;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}

gboolean
mu_msg_iter_reset (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, FALSE);

	iter->set_msg (NULL);

	try {
		iter->set_cursor(iter->matches().begin());

	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);

	return TRUE;
}

gboolean
mu_msg_iter_next (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, FALSE);

	iter->set_msg (NULL);

	if (mu_msg_iter_is_done(iter))
		return FALSE;

	try {
		iter->cursor_next();

		if (iter->cursor() == iter->matches().end())
			return FALSE;

		if ((iter->skip_unreadable() && !is_msg_file_readable (iter)) ||
		    (iter->skip_dups() && iter->looks_like_dup ()))
			return mu_msg_iter_next (iter); /* skip! */

		return TRUE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}


gboolean
mu_msg_iter_is_done (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, TRUE);

	try {
		return iter->cursor() == iter->matches().end() ? TRUE : FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (TRUE);
}



/* hmmm.... is it impossible to get a 0 docid, or just very improbable? */
unsigned
mu_msg_iter_get_docid (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, (unsigned int)-1);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter),
			      (unsigned int)-1);
	try {
		return iter->docid();

	} MU_XAPIAN_CATCH_BLOCK_RETURN ((unsigned int)-1);
}



char*
mu_msg_iter_get_msgid (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);

	try {
		return g_strdup (iter->msgid().c_str());

	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}


char**
mu_msg_iter_get_refs (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);

	try {
		std::string refs (
			iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_REFS));
		if (refs.empty())
			return NULL;
		return g_strsplit (refs.c_str(),",", -1);

	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}

char*
mu_msg_iter_get_thread_id (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);

	try {
		const std::string thread_id (
			iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_THREAD_ID).c_str());
		
		return thread_id.empty() ? NULL : g_strdup (thread_id.c_str());


	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}


const MuMsgIterThreadInfo*
mu_msg_iter_get_thread_info (MuMsgIter *iter)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);

	/* maybe we don't have thread info */
	if (!iter->thread_hash())
		return NULL;

	try {
		const MuMsgIterThreadInfo *ti;
		unsigned int docid;

		docid = mu_msg_iter_get_docid (iter);
		ti    = (const MuMsgIterThreadInfo*)g_hash_table_lookup
			(iter->thread_hash(), GUINT_TO_POINTER(docid));

		if (!ti)
			g_warning ("no ti for %u\n", docid);

		return ti;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}
