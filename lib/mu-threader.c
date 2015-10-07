/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <math.h>   /* for log, ceil */
#include <string.h> /* for memset */

#include "mu-threader.h"
#include "mu-container.h"
#include "mu-str.h"

/* msg threading implementation based on JWZ's algorithm, as described in:
 *    http://www.jwz.org/doc/threading.html
 *
 * the implementation follows the terminology from that doc, so should
 * be understandable from that... I did change things a bit though
 *
 * the end result of the threading operation is a hashtable which maps
 * docids (ie., Xapian documents == messages) to 'thread paths'; a
 * thread path is a string denoting the 2-dimensional place of a
 * message in a list of messages,
 *
 * Msg1                        => 00000
 * Msg2                        => 00001
 *   Msg3 (child of Msg2)      => 00001:00000
 *   Msg4 (child of Msg2)      => 00001:00001
 *     Msg5 (child of Msg4)    => 00001:00001:00000
 * Msg6                        => 00002
 *
 * the padding-0's are added to make them easy to sort using strcmp;
 * the number hexadecimal numbers, and the length of the 'segments'
 * (the parts separated by the ':') is equal to ceil(log_16(matchnum))
 *
 */

/* step 1 */ static GHashTable* create_containers (MuMsgIter *iter);
/* step 2 */ static MuContainer *find_root_set (GHashTable *ids);
static MuContainer* prune_empty_containers (MuContainer *root);
/* static void group_root_set_by_subject (GSList *root_set); */
GHashTable* create_doc_id_thread_path_hash (MuContainer *root,
					    size_t match_num);

/* msg threading algorithm, based on JWZ's algorithm,
 * http://www.jwz.org/doc/threading.html */
GHashTable*
mu_threader_calculate (MuMsgIter *iter, size_t matchnum,
		       MuMsgFieldId sortfield, gboolean descending)
{
	GHashTable *id_table, *thread_ids;
	MuContainer *root_set;

	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfield) ||
			      sortfield == MU_MSG_FIELD_ID_NONE,
			      FALSE);

	/* step 1 */
	id_table = create_containers (iter);
	if (matchnum == 0)
		return id_table; /* just return an empty table */

	/* step 2 -- the root_set is the list of children without parent */
	root_set = find_root_set (id_table);

	/* step 3: skip until the end; we still need to containers */

	/* step 4: prune empty containers */
	root_set = prune_empty_containers (root_set);

	/* sort root set */
	if (sortfield != MU_MSG_FIELD_ID_NONE)
		root_set = mu_container_sort (root_set, sortfield, descending,
					      NULL);

	/* step 5: group root set by subject */
	/* group_root_set_by_subject (root_set); */

	/* sort */
	mu_msg_iter_reset (iter); /* go all the way back */

	/* finally, deliver the docid => thread-path hash */
	thread_ids = mu_container_thread_info_hash_new (root_set,
							matchnum);

	g_hash_table_destroy (id_table); /* step 3*/

	return thread_ids;
}

G_GNUC_UNUSED static void
check_dup (const char *msgid, MuContainer *c, GHashTable *hash)
{
	if (g_hash_table_lookup (hash, c)) {
		g_warning ("ALREADY!!");
		mu_container_dump (c, FALSE);
		g_assert (0);
	} else
		g_hash_table_insert (hash, c, GUINT_TO_POINTER(TRUE));
}


G_GNUC_UNUSED static void
assert_no_duplicates (GHashTable *ids)
{
	GHashTable *hash;

	hash = g_hash_table_new (g_direct_hash, g_direct_equal);

	g_hash_table_foreach (ids, (GHFunc)check_dup, hash);

	g_hash_table_destroy (hash);
}


/* a referred message is a message that is refered by some other
 * message */
static MuContainer*
find_or_create_referred (GHashTable *id_table, const char *msgid,
			 gboolean *created)
{
	MuContainer *c;

	g_return_val_if_fail (msgid, NULL);

	c = g_hash_table_lookup (id_table, msgid);
	*created = !c;
	if (!c) {
		c = mu_container_new (NULL, 0, msgid);
		g_hash_table_insert (id_table, (gpointer)msgid, c);
		/* assert_no_duplicates (id_table); */
	}


	return c;
}

/* find a container for the given msgid; if it does not exist yet,
 * create a new one, and register it */
static MuContainer*
find_or_create (GHashTable *id_table, MuMsg *msg, guint docid)
{
	MuContainer	*c;
	const char*	 msgid;
	char		 fake[32];
	
	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (docid != 0, NULL);

	msgid = mu_msg_get_msgid (msg);
	if (!msgid)
		msgid = mu_msg_get_path (msg); /* fake it */
	if (!msgid) { /* no path either? seems to happen... */
		g_warning ("message without path");
		snprintf (fake, sizeof(fake), "fake:%p", (gpointer)msg);
		msgid = fake;
	}
	
	/* XXX the '<none>' works around a crash; find a better
	 * solution */
	c = g_hash_table_lookup (id_table, msgid);

	/* If id_table contains an empty MuContainer for this ID: * *
	 * Store this message in the MuContainer's message slot. */
	if (c) {
		if (!c->msg) {
			c->msg	  = mu_msg_ref (msg);
			c->docid  = docid;
			return c;
		} else {
			/* special case, not in the JWZ algorithm: the
			 * container exists already and has a message; this
			 * means that we are seeing *another message* with a
			 * message-id we already saw... create this message,
			 * and mark it as a duplicate, and a child of the one
			 * we saw before; use its path as a fake message-id
			 * */
			MuContainer *c2;
			const char* fake_msgid;

			fake_msgid = mu_msg_get_path (msg);

			c2	  = mu_container_new (msg, docid, fake_msgid);
			c2->flags = MU_CONTAINER_FLAG_DUP;
			/*c	  = */ mu_container_append_children (c, c2);

			g_hash_table_insert (id_table, (gpointer)fake_msgid, c2);

			return NULL; /* don't process this message further */
		}
	} else { /* Else: Create a new MuContainer object holding
		    this message; Index the MuContainer by
		    Message-ID in id_table. */
		c = mu_container_new (msg, docid, msgid);
		g_hash_table_insert (id_table, (gpointer)msgid, c);
		/* assert_no_duplicates (id_table); */

		return c;
	}
}

static gboolean
child_elligible (MuContainer *parent, MuContainer *child, gboolean created)
{
	if (!parent || !child)
		return FALSE;
	if (child->parent)
		return FALSE;
	/* if (created) */
	/* 	return TRUE; */
	if (mu_container_reachable (parent, child))
		return FALSE;
	if (mu_container_reachable (child, parent))
		return FALSE;

	return TRUE;
}



static void /* 1B */
handle_references (GHashTable *id_table, MuContainer *c)
{
	const GSList *refs, *cur;
	MuContainer *parent;
	gboolean created;

	refs = mu_msg_get_references (c->msg);
	if (!refs)
		return; /* nothing to do */

	/* For each element in the message's References field:

	   Find a MuContainer object for the given Message-ID: If
	   there's one in id_table use that; Otherwise, make (and
	   index) one with a null Message. */

	/* go over over our list of refs, until 1 before the last... */
	created = FALSE;
	for (parent = NULL, cur = refs; cur; cur = g_slist_next (cur)) {

		MuContainer *child;
		child = find_or_create_referred (id_table, (gchar*)cur->data,
						 &created);

		/* if we find the current message in their own refs, break now
		   so that parent != c in next step */
		if (child == c)
			break;

		/*Link the References field's MuContainers together in
		 * the order implied by the References header.

		 If they are already linked, don't change the existing
		 links.  Do not add a link if adding that link would
		 introduce a loop: that is, before asserting A->B,
		 search down the children of B to see if A is
		 reachable, and also search down the children of A to
		 see if B is reachable. If either is already reachable
		 as a child of the other, don't add the link. */

		if (child_elligible (parent, child, created))
			/*parent =*/
			mu_container_append_children (parent, child);

		parent = child;
	}

	/* 'parent' points to the last ref: our direct parent;

	   Set the parent of this message to be the last element in
	   References. Note that this message may have a parent
	   already: this can happen because we saw this ID in a
	   References field, and presumed a parent based on the other
	   entries in that field. Now that we have the actual message,
	   we can be more definitive, so throw away the old parent and
	   use this new one. Find this MuContainer in the parent's
	   children list, and unlink it.

	   Note that this could cause this message to now have no
	   parent, if it has no references field, but some message
	   referred to it as the non-first element of its
	   references. (Which would have been some kind of lie...)

	   Note that at all times, the various ``parent'' and ``child'' fields
	   must be kept inter-consistent. */

        /* optimization: if the the message was newly added, it's by
   	   definition not reachable yet */

	/* So, we move c and its descendants to become a child of parent if:
	   * both are not NULL
	   * parent is not a descendant of c.
	   * both are different from each other (guaranteed in last loop) */

	if (parent && c && !(c->child && mu_container_reachable (c->child, parent))) {

		/* if c already has a parent, remove c from its parent children
		   and reparent it, as now we know who is c's parent reliably */
		if (c->parent) {
			mu_container_remove_child(c->parent, c);
			c->next = c->last = c->parent = NULL;
		}

		/*parent = */mu_container_append_children (parent, c);
	}
}



/* step 1: create the containers, connect them, and fill the id_table */
static GHashTable*
create_containers (MuMsgIter *iter)
{
	GHashTable *id_table;
	id_table = g_hash_table_new_full (g_str_hash, g_str_equal,
					  NULL,
					  (GDestroyNotify)mu_container_destroy);

	for (mu_msg_iter_reset (iter); !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next (iter)) {

		MuContainer *c;
		MuMsg *msg;
		unsigned docid;

		/* 1.A */
		msg   = mu_msg_iter_get_msg_floating (iter); /* don't unref */
		docid = mu_msg_iter_get_docid (iter);

		c = find_or_create (id_table, msg, docid);

		/* 1.B and C */
		if (c)
			handle_references (id_table, c);
	}

	return id_table;
}



static void
filter_root_set (const gchar *msgid, MuContainer *c, MuContainer **root_set)
{
	/* ignore children */
	if (c->parent)
		return;

	/* ignore duplicates */
	if (c->flags & MU_CONTAINER_FLAG_DUP)
		return;

	if (*root_set == NULL) {
		*root_set = c;
		return;
	} else
		*root_set = mu_container_append_siblings (*root_set, c);
}


/* 2.  Walk over the elements of id_table, and gather a list of the
   MuContainer objects that have no parents, but do have children */
static MuContainer*
find_root_set (GHashTable *ids)
{
	MuContainer *root_set;

	root_set = NULL;
	g_hash_table_foreach (ids, (GHFunc)filter_root_set, &root_set);

	return root_set;
}


static gboolean
prune_maybe (MuContainer *c)
{
	MuContainer *cur;

	for (cur = c->child; cur; cur = cur->next) {
		if (cur->flags & MU_CONTAINER_FLAG_DELETE) {
			c = mu_container_remove_child (c, cur);
		} else if (cur->flags & MU_CONTAINER_FLAG_SPLICE) {
			c = mu_container_splice_grandchildren (c, cur);
			c = mu_container_remove_child (c, cur);
		}
	}

	g_return_val_if_fail (c, FALSE);

	/* don't touch containers with messages */
	if (c->msg)
		return TRUE;

	/* A. If it is an msg-less container with no children, mark it for
	 * deletion. */
	if (!c->child) {
		c->flags |= MU_CONTAINER_FLAG_DELETE;
		return TRUE;
	}

	/* B. If the MuContainer has no Message, but does have
	 * children, remove this container but promote its
	 * children to this level (that is, splice them in to
	 * the current child list.)
	 *
	 * Do not promote the children if doing so would
	 * promote them to the root set -- unless there is
	 * only one child, in which case, do.
	 */
	if (c->child->next) /* ie., > 1 child */
		return TRUE;

	c->flags |= MU_CONTAINER_FLAG_SPLICE;

	return TRUE;
}


static MuContainer*
prune_empty_containers (MuContainer *root_set)
{
	MuContainer *cur;

	mu_container_foreach (root_set,
			      (MuContainerForeachFunc)prune_maybe,
			      NULL);

	/* and prune the root_set itself... */
	for (cur = root_set; cur; cur = cur->next) {
		if (cur->flags & MU_CONTAINER_FLAG_DELETE) {
			root_set = mu_container_remove_sibling (root_set, cur);
		} else if (cur->flags & MU_CONTAINER_FLAG_SPLICE) {
			root_set = mu_container_splice_children (root_set, cur);
			root_set = mu_container_remove_sibling (root_set, cur);
		}
	}

	return root_set;
}
