/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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

#include "mu-msg-threader.h"

struct _MuMsgThreader {
	GHashTable *_ids;
};

struct _Container {
	MuMsg *_msg;
	struct _Container *_parent;
	GSList *_children;
};
typedef struct _Container Container;

static Container *container_new       (MuMsg *msg);
static void       container_destroy   (Container *c);


/* breath-first recursive traversal */
typedef gboolean (*ContainerTraverseFunc) (Container *c, gpointer user_data);
static gboolean  container_traverse  (Container *c, ContainerTraverseFunc func,
				      gpointer user_data);
static gboolean  container_traverse_list (GSList *containers, ContainerTraverseFunc func,
					  gpointer user_data);
static void       container_add_child (Container *c, Container *child);

MuMsgThreader *
mu_msg_threader_new (void)
{
	MuMsgThreader *self;

	self = g_slice_new (MuMsgThreader);
	self->_ids = g_hash_table_new_full (g_str_hash,
					    g_str_equal,
					    NULL, /* we don't copy msgid */
					    (GDestroyNotify)container_destroy);
	return self;
}

void
mu_msg_threader_destroy (MuMsgThreader *self)
{
	if (!self)
		return;

	g_hash_table_destroy (self->_ids);
	
	g_slice_free (MuMsgThreader, self);
}


static Container *store_msg_in_container (GHashTable *ids, MuMsg *msg);
static void handle_references (GHashTable *ids, Container *c);
static Container* find_or_create (GHashTable *ids, char *msgid);
static GSList *find_root_set (GHashTable *ids);

/* msg threading algorithm, based on JWZ's algorithm,
 * http://www.jwz.org/doc/threading.html */
gboolean
mu_msg_threader_calculate (MuMsgThreader *self, MuMsgIter *iter)
{
	g_return_val_if_fail (iter, FALSE);

	/* 1. for all messages... */
	for (mu_msg_iter_reset (iter); !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next (iter)) {

		Container *c;
		GSList *root_set;
		
		/* 1.A */
		c = store_msg_in_container (self->_ids,
					    mu_msg_iter_get_msg (iter, NULL));
		if (!c) {
			g_printerr ("failed to create container\n");
			continue;
		}
		
		/* 1.B and C */
		handle_references (self->_ids, c);

		/* step 2 */
		root_set = find_root_set (self->_ids);
		{
			GSList *cur;
			for (cur = root_set; cur; cur = g_slist_next (cur)) {
				MuMsg *msg;
				msg = ((Container*)cur->data)->_msg;
				if (!msg) 
					g_printerr ("<empty root>\n");
				else {
					const gchar *subj;
					subj = mu_msg_get_subject (msg);
					g_printerr ("%s\n", subj ? subj : "<no subject>");
				}	
			}
			
		}


		
	}

	mu_msg_iter_reset (iter); /* go all the way back */

	return TRUE;
}


static Container* /* 1A */
store_msg_in_container (GHashTable *ids, MuMsg *msg)
{
	/* 1.A */
	Container *c;
	const char *msgid;
	
	msgid = mu_msg_get_msgid (msg);

	g_return_val_if_fail (msgid, NULL);
	
	c = find_or_create (ids, (gchar*)msgid);
	
	if (!c->_msg)
		c->_msg = mu_msg_ref (msg);
	else
		g_printerr ("%s: duplicate msgid; ignore\n",
			__FUNCTION__); /* FIXME */
	return c;
}



static void
set_parent_child (Container *parent, Container *child)
{
	/* FIXME: set relationship, but first check if they're not
	 * already linked */
	
	container_add_child (parent, child);
	child->_parent = parent;
	
	g_printerr ("%s: %p is a parent of %p\n", __FUNCTION__,
		    (gpointer)parent, (gpointer)child);
}



static void /* 1B */
handle_references (GHashTable *ids, Container *c)
{
	const gchar *refsstr;
	gchar **refs, **cur;

	refsstr = mu_msg_get_references_str (c->_msg);
	if (!refsstr)
		return; /* nothing to do */
	
	refs = g_strsplit (refsstr,",",-1);
	
	/* go over over our list of refs */ 
	for (cur = refs; *cur && *(cur + 1); ++cur) {
		Container *c1, *c2; /* two consecutive refs in the list;
				     * we register them as parent, child */
		c1 = find_or_create (ids, *cur);
		c2 = find_or_create (ids, *(cur + 1));

		set_parent_child (c1, c2);
	}
	
	/* now cur points to the final ref; we register that with
	 * ourselves...*/
	set_parent_child (find_or_create (ids, *cur), c);

	g_strfreev (refs);
}


	
/* find a container for the given msgid; if it does not exist yet,
 * create a new one, and register it */
static Container*
find_or_create (GHashTable *ids, char *msgid)
{
	Container *c;

	g_return_val_if_fail (msgid, NULL);
	
	c = g_hash_table_lookup (ids, msgid);
	if (c) { /* we found the container? */
		g_printerr ("%s: found %s\n", __FUNCTION__, msgid);
		return c;
	}
		
	/* no container yet, create it */
	c = container_new (NULL);
	
	/* no need to copy msgid, as the MuMsg will be around still,
	 * owning the const gchar* FIXME we must g_strdup for some
	 * reason */
	g_hash_table_insert (ids, g_strdup(msgid), c);
	g_printerr ("%s: created %s\n", __FUNCTION__, msgid);
	
	return c;
}

static void
filter_parentless (const gchar *msgid, Container *c, GSList **lst)
{
	if (!c->_parent)
		*lst = g_slist_prepend (*lst, c);
}



static GSList *
find_root_set (GHashTable *ids)
{
	GSList *lst;

	lst = NULL;
	g_hash_table_foreach (ids, (GHFunc)filter_parentless, &lst);

	return lst;
}




static Container*
container_new (MuMsg *msg)
{
	Container *c;

	c = g_slice_new0 (Container);
	if (msg)
		c->_msg = mu_msg_ref (msg);

	return c;
}

static void
container_destroy (Container *c)
{
	if (!c)
		return;

	if (c->_msg)
		mu_msg_unref (c->_msg);

	//g_slice_free (Container, c);
}


/* depth-first traverse all children, grand-children ... n-children of
 * container */
static gboolean
container_traverse (Container *c, ContainerTraverseFunc func,
		    gpointer user_data)
{
	/* if func returns FALSE, quit */
	if (!func (c, user_data))
		return FALSE;

	if (c->_children)
		return container_traverse_list (c->_children,
						func, user_data);
	return TRUE;
}


static gboolean
container_traverse_list (GSList *containers, ContainerTraverseFunc func,
			 gpointer user_data)
{
	GSList *cur;
	
	for (cur = containers; cur; cur = g_slist_next (cur)) 	
		if (!container_traverse ((Container*)cur->data, func, user_data))
			return FALSE; /* quit */
	return TRUE;
}


static void
container_add_child (Container *c, Container *child)
{
	g_return_if_fail (c != child);
	g_return_if_fail (child);

	c->_children = g_slist_prepend (c->_children, child);
}

