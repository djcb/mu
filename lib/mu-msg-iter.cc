/* -*- mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-util.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"
#include "mu-threader.h"

/* just a guess... */
#define MAX_FETCH_SIZE 10000

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
		_enq(enq), _thread_hash (0), _msg(0), _flags(flags) {

		bool threads, descending;

		threads     = (flags & MU_MSG_ITER_FLAG_THREADS);
		descending  = (flags & MU_MSG_ITER_FLAG_DESCENDING);

		_matches = _enq.get_mset (0, maxnum);

		/* when threading, we calculate the threads for the
		 * set of matches, then requery/sort based on the
		 * threading */
		if (threads && !_matches.empty()) {

			_matches.fetch();
			_thread_hash = mu_threader_calculate
				(this, _matches.size(), sortfield,
				 descending ? TRUE: FALSE);

			ThreadKeyMaker keymaker(_thread_hash);

			enq.set_sort_by_key (&keymaker, false);
			_matches = _enq.get_mset (0, maxnum);
		}

		_cursor	 = _matches.begin();

		/* this seems to make search slightly faster, some
		 * non-scientific testing suggests. 5-10% or so */
		if (_matches.size() <= MAX_FETCH_SIZE)
			_matches.fetch ();
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

	bool msg_uid_seen_before (const std::string& msg_uid) {
		if (_msg_uid_set.count (msg_uid) > 0)
			return true;
		_msg_uid_set.insert (msg_uid);
		return false;
	}

private:
	const Xapian::Enquire		_enq;
	Xapian::MSet			_matches;
	Xapian::MSet::const_iterator	_cursor;

	GHashTable      *_thread_hash;
	MuMsg		*_msg;

	MuMsgIterFlags _flags;

	struct ltstr {
		bool operator () (const std::string &s1, const std::string &s2) const {
			return g_strcmp0 (s1.c_str(), s2.c_str());
		}
	};
	std::set <std::string, ltstr> _msg_uid_set;
};


static gboolean
is_msg_file_readable (MuMsgIter *iter)
{
	std::string path
		(iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_PATH));

	if (path.empty())
		return FALSE;

	return (access (path.c_str(), R_OK) == 0) ? TRUE : FALSE;
}



MuMsgIter *
mu_msg_iter_new (XapianEnquire *enq, size_t maxnum,
		 MuMsgFieldId sortfield, MuMsgIterFlags flags,
		 GError **err)
{
	g_return_val_if_fail (enq, NULL);
	/* sortfield should be set to .._NONE when we're not threading */
	g_return_val_if_fail ((flags & MU_MSG_ITER_FLAG_THREADS)
			      || sortfield == MU_MSG_FIELD_ID_NONE,
			      NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfield) ||
			      sortfield == MU_MSG_FIELD_ID_NONE,
			      FALSE);
	try {
		MuMsgIter *iter;
		iter = new MuMsgIter ((Xapian::Enquire&)*enq,
				      maxnum,
				      sortfield,
				      flags);
		if ((flags & MU_MSG_ITER_FLAG_SKIP_UNREADABLE) &&
		    !is_msg_file_readable (iter))
			mu_msg_iter_next (iter);

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

static gboolean
msg_seen_before (MuMsgIter *iter)
{
	// we *only* consider the msgid; we could also look eg. the
	// date, but it seems that Gmail may change the date / time
	// zone etc. with still being the same message.
	const std::string msg_uid
		(iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_MSGID));
		 // (iter->cursor().get_document().get_value(MU_MSG_FIELD_ID_DATE)));
	if (msg_uid.empty())
		return FALSE;
	else
		return iter->msg_uid_seen_before (msg_uid);
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
		/* filter out non-existing messages? */
		else if ((iter->flags() &
			  MU_MSG_ITER_FLAG_SKIP_UNREADABLE) &&
			 !is_msg_file_readable (iter))
			return mu_msg_iter_next (iter); /*skip!*/
		/* filter out msgid duplicates? */
		else if ((iter->flags() & MU_MSG_ITER_FLAG_SKIP_DUPS) &&
			 msg_seen_before (iter))
			return mu_msg_iter_next (iter); /*skip!*/
		else
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
unsigned int
mu_msg_iter_get_docid (MuMsgIter *iter)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter),
			      (unsigned int)-1);
	try {
		return iter->cursor().get_document().get_docid();

	} MU_XAPIAN_CATCH_BLOCK_RETURN (0);
}



const MuMsgIterThreadInfo*
mu_msg_iter_get_thread_info (MuMsgIter *iter)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);
	g_return_val_if_fail (iter->thread_hash(), NULL);

	try {
		const MuMsgIterThreadInfo *ti;
		unsigned int docid;

		docid = mu_msg_iter_get_docid (iter);
		ti    = (const MuMsgIterThreadInfo*)g_hash_table_lookup
			(iter->thread_hash(), GUINT_TO_POINTER(docid));

		if (!ti)
			g_printerr ("no ti for %u\n", docid);

		return ti;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}
