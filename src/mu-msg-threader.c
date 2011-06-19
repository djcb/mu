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
 * the padding-0's are added to make them easy to sort using strcmp
 * 
 */

/* Container data structure, as seen in the JWZ-doc; one differences
 * is that I use GSLists for the children, rather than 'next'
 * pointers
 *
 * the _state is for pruning; when traversing the tree, i mark
 * containers with NUKE or SPLICE, and then do it afterwards; that
 * way, I don't have to change the very list I am iterating over...
 *
 * */
enum _ContainerState { NUKE, SPLICE, OKAY };
typedef enum _ContainerState ContainerState;

struct _Container {
	MuMsg			*_msg;
	unsigned int		_docid;
	struct _Container	*_parent;
	GSList			*_children;
	ContainerState		_state;
};
typedef struct _Container	 Container;

static Container *container_new       (MuMsg *msg, unsigned docid);
static void       container_destroy   (Container *c);
static void       container_dump (Container *c);
/* breath-first recursive traversal */
typedef gboolean (*ContainerTraverseFunc) (Container *c, guint level,
					   gpointer user_data);
static gboolean  container_traverse  (Container *c,
				      guint level,
				      ContainerTraverseFunc func,
				      gpointer user_data);
static gboolean container_add_child (Container *c, Container *child);
static void container_promote_child (Container *c, Container *child);
static void container_remove_child (Container *c, Container *child);





/* step 1 */ static GHashTable* create_containers (MuMsgIter *iter);
/* step 2 */ static GSList *find_root_set (GHashTable *ids);
static void prune_empty_containers (GSList *root_set);
/* static void group_root_set_by_subject (GSList *root_set); */
GHashTable* create_doc_id_thread_path_hash (GSList *root_set, size_t match_num);

/* msg threading algorithm, based on JWZ's algorithm,
 * http://www.jwz.org/doc/threading.html */
GHashTable*
mu_msg_threader_calculate (MuMsgIter *iter, size_t matchnum)
{
	GHashTable *id_table, *thread_ids;
	GSList *root_set;

	g_return_val_if_fail (iter, FALSE);

	/* step 1 */
	id_table = create_containers (iter);
	
	/* step 2 */
	root_set = find_root_set (id_table);
	
	/* step 3: skip until the end; we still need to containers */

	/* step 4: prune empty containers */
	prune_empty_containers (root_set);

	/* recalculate root set */
	g_slist_free (root_set);
	root_set = find_root_set (id_table);
	
	/* step 5: group root set by subject */
//	group_root_set_by_subject (root_set);

	/* sort */
	
	
	mu_msg_iter_reset (iter); /* go all the way back */
	
	/* finally, deliver the docid => thread-path hash */
	thread_ids =  create_doc_id_thread_path_hash (root_set, matchnum);
	g_hash_table_destroy (id_table); /* step 3*/

	g_slist_free (root_set);
	return thread_ids;
}

static gboolean
check_exists (Container *c, guint level, Container *c2)
{
	return c != c2;
}


static gboolean
already_referenced (Container *c1, Container *c2)
{
	gboolean notfound;
	notfound = container_traverse ((Container*)c1, 0,
				       (ContainerTraverseFunc)check_exists,
				       c2);

	return notfound ? FALSE : TRUE;
}


/* find a container for the given msgid; if it does not exist yet,
 * create a new one, and register it */
static Container*
find_or_create (GHashTable *id_table, const char* msgid, unsigned docid)
{
	Container *c;

	c = g_hash_table_lookup (id_table, msgid);
	
	if (!c) {
		c = container_new (NULL, docid);
		g_hash_table_insert (id_table, (gpointer)msgid, c);
	}

	return c;
}





static void /* 1B */
handle_references (GHashTable *id_table, Container *c)
{
	const GSList *refs, *cur;
	
	refs = mu_msg_get_references (c->_msg);
	if (!refs)
		return; /* nothing to do */
	
	/* go over over our list of refs */ 
	for (cur = refs; cur && cur->next; cur = g_slist_next (cur)) {
		Container *c1, *c2; /* two consecutive refs in the list;
				     * we register them as parent, child */
		c1 = find_or_create (id_table, (gchar*)cur->data, 0);
		c2 = find_or_create (id_table, (gchar*)cur->next->data, 0);
		
		container_add_child (c1, c2);
	}
	
	/* now cur points to the final ref, which refers to our own
	 * parent... register it */
	if (cur) {
		Container *parent;
		parent = find_or_create (id_table, (gchar*)cur->data, 0);
		container_add_child (parent, c);
	}
}



/* step 1: create the containers, connect them, and fill the id_table */
static GHashTable*
create_containers (MuMsgIter *iter)
{
	GHashTable *id_table;
	id_table = g_hash_table_new_full (g_str_hash,
					  g_str_equal,
					  NULL, /* we don't copy msgid */
					  (GDestroyNotify)container_destroy);
	
	for (mu_msg_iter_reset (iter); !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next (iter)) {

		Container *c;
		MuMsg *msg;
		unsigned docid;
		const char *msgid;
			
		/* 1.A */
		msg = mu_msg_iter_get_msg (iter, NULL);
		msgid = mu_msg_get_msgid (msg);
		docid = mu_msg_iter_get_docid (iter);
				
		if (!msgid) {
			const char* path;
			path = mu_msg_get_path(msg);
			/* g_warning ("msg without msgid %s", path); */
			msgid = path; /* fake it... */
		}
		
		c = find_or_create (id_table, msgid, docid);
		if (!c->_msg)
			c->_msg = mu_msg_ref (msg);
		
		/* 1.B and C */
		handle_references (id_table, c);		
	}	


	return id_table;
}



static void
filter_root_set (const gchar *msgid, Container *c, GSList **lst)
{
	if (!c->_parent && c->_state != NUKE)
		*lst = g_slist_prepend (*lst, c);
}



/* 2. Find the root set.  Walk over the elements of id_table, and
   gather a list of the Container objects that have no parents, but do
   have children */
static GSList*
find_root_set (GHashTable *ids)
{
	GSList *lst;

	lst = NULL;
	g_hash_table_foreach (ids, (GHFunc)filter_root_set, &lst);

	return lst;
}



static void
do_pruning (GSList *containers)
{
	GSList *cur;
	
	/* now, do stuff to our children... */
	for (cur = containers; cur; cur = g_slist_next(cur)) {
		
		Container *child;
		child = (Container *)cur->data;
				
		if (child->_state == SPLICE) {
			/* g_printerr ("SPLICE %p\n", (void*)child); */
			container_promote_child (child->_parent, child);
			//container_remove_child (child->_parent, child);

		} else if (child->_state == NUKE) {
			/* g_printerr ("NUKE %p\n", (void*)child); */
			container_remove_child (child->_parent, child);
 		}

		child->_state = OKAY;
	}
}

/* this function will mark 'containers', and the do the pruning on
 * their children */
static void 
prune_empty_nonroot (GSList *containers)
{
	GSList *cur;
	
	for (cur = containers; cur; cur = g_slist_next (cur)) {

		Container *container;
			
		container = (Container*)cur->data;

		if (container->_children) {
			prune_empty_nonroot (container->_children); /* recurse! */
			do_pruning (container->_children);
		}

		/* don't touch containers with messages */
		if (container->_msg)
			continue; 
		
		/* A. If it is an msg-less container with no children, nuke it. */
		if (!container->_children) 
			container->_state = NUKE;
		
		/* B. If the Container has no Message, but does have
		 * children, remove this container but promote its
		 * children to this level (that is, splice them in to
		 * the current child list.)
		 *
		 * Do not promote the children if doing so would
		 * promote them to the root set -- unless there is
		 * only one child, in which case, do. */
		else if ((container->_parent ||
			  g_slist_length(container->_children) == 1))
			container->_state = SPLICE;	
	}
}


/* 4. Prune empty containers */
static void 
prune_empty_containers (GSList *root_set)
{
	GSList *cur;
	
	/* everything below the root_set will be pruned */
	prune_empty_nonroot (root_set);

	/* no, clear up the root_set itself... */
	for (cur = root_set; cur; cur = g_slist_next(cur)) {
		
		Container *c;
		c = (Container *)cur->data;
		
		if (c->_state == SPLICE) {
			/* make child parent-less, so the become part of the root_set */
			GSList *iter; /* there should be only 1... */
			for (iter = c->_children; iter; iter = g_slist_next(iter))
				((Container*)iter->data)->_parent = NULL;
			c->_children = NULL;
			c->_state = NUKE;
		}
	}
}


#if 0
/* 5. group root set by subject */
static void
group_root_set_by_subject (GSList *root_set)
{
	GHashTable *subject_table;
	GSList *cur;
	
	/* A: Construct a new hash table, subject_table, which
	 * associates subject strings with Container objects. */

	subject_table = g_hash_table_new (g_str_hash, g_str_equal);

	for (cur = root_set; cur; cur = g_slist_next (cur)) {

		const char *subject, *subj;
		/* subject without Re: Fwd: etc. */
		
		/* Find the subject of that sub-tree: */
		Container *c;
		c = (Container*)cur->data;
		

		if (c->_msg) 
			/* (i) if there is a message in the Container, the
			 * subject is the subject of that message. */ 
			subject = mu_msg_get_subject (c->_msg);
		else
			/* (ii )If there is no message in the Container,
			 * then the Container will have at least one
			 * child Container, and that Container will
			 * have a message. Use the subject of that
			 * message instead. */
			subject = mu_msg_get_subject (
				((Container*)(c->_children->data))->_msg);
		/* (iii) Strip ``Re:'', ``RE:'', ``RE[5]:'', ``Re:
		 * Re[4]: Re:'' and so on. */ 
		subj = subject ? mu_str_subject_normalize (subject) : NULL;
	
		/* (iv )If the subject is now "", give up on this
		 * Container. */
		if (mu_str_is_empty (subj))
			continue;
	}		
}

#endif

struct _ThreadInfo {
	GHashTable *hash;
	GQueue *idqueue;
	unsigned prev_level;
};
typedef struct _ThreadInfo ThreadInfo;

static void
accumulate_path (int seq, char **threadpath)
{
	if (*threadpath) {
		char *path;
		path = g_strdup_printf ("%s:%05d", *threadpath, seq);
		g_free (*threadpath);
		*threadpath = path;
	} else 
		*threadpath = g_strdup_printf ("%05d", seq);
}

/* let's make a GtkTreePath compatible thread path */
static gboolean
add_thread_path (Container *c, guint level, ThreadInfo *ti)
{
	gchar *threadpath;
	unsigned i;
	
	if (level > ti->prev_level) {
		for (i = ti->prev_level; i != level; ++i)
			g_queue_push_tail (ti->idqueue, GUINT_TO_POINTER(0));
	} else if (level <= ti->prev_level) {
		int oldseq;
		
		/* level == ti->prev_level, the for-loop is void */
		for (i = level; i != ti->prev_level; ++i)
			(void)g_queue_pop_tail (ti->idqueue);

		oldseq = GPOINTER_TO_UINT(g_queue_pop_tail(ti->idqueue));
		g_queue_push_tail (ti->idqueue,GUINT_TO_POINTER(1 + oldseq));
	}
	
	threadpath = NULL;
	g_queue_foreach (ti->idqueue, (GFunc)accumulate_path, &threadpath);
	if (c->_docid) /* don't put empty (pseudo) in the hash */
		g_hash_table_insert (ti->hash, GUINT_TO_POINTER(c->_docid),
			     threadpath);
	else
		g_free (threadpath);
	
	ti->prev_level = level;

	return TRUE;
}



GHashTable*
create_doc_id_thread_path_hash (GSList *root_set, size_t matchnum)
{
	ThreadInfo ti;
	GSList *cur;
	int i;
	
	/* create hash docid => thread-path */
	ti.hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
					 NULL,
					 (GDestroyNotify)g_free);

	ti.idqueue    = g_queue_new ();
	g_queue_push_tail (ti.idqueue, GUINT_TO_POINTER(0));

	ti.prev_level = 0;
	
	for (i = 0, cur = root_set; cur; cur = g_slist_next (cur)) 
		container_traverse ((Container*)cur->data, 0,
				    (ContainerTraverseFunc)add_thread_path,
				    &ti);

	g_queue_free (ti.idqueue);
	return ti.hash;
}


static Container*
container_new (MuMsg *msg, unsigned docid)
{
	Container *c;

	c = g_slice_new0 (Container);
	if (msg)
		c->_msg = mu_msg_ref (msg);
	
	c->_docid = docid;
	c->_state = OKAY;
	
	return c;
}

static void
container_destroy (Container *c)
{
	if (!c)
		return;

	if (c->_msg)
		mu_msg_unref (c->_msg);

	/* free the list, not the children */
	g_slist_free (c->_children);
	
	g_slice_free (Container, c);
}


/* depth-first traverse all children, grand-children ... n-children of
 * container */
static gboolean
container_traverse (Container *c, guint level, ContainerTraverseFunc func,
		    gpointer user_data) 
{
	GSList *cur;
	int i;
	
	g_return_val_if_fail (c, FALSE);

	if (!func (c, level, user_data))
		return FALSE;
	
	for (i = 0, cur = c->_children; cur; cur = g_slist_next (cur)) {
		if (!container_traverse ((Container*)cur->data, level + 1,
					 func, user_data))
			return FALSE;
		
	}
	return TRUE;
}


static gboolean
container_add_child (Container *parent, Container *child)
{
	g_return_val_if_fail (parent != child, FALSE);
	g_return_val_if_fail (child, FALSE);

	if (already_referenced(child, parent) ||
	    already_referenced(parent, child))
		return FALSE;
	    
	child->_parent	  = parent;
	parent->_children = g_slist_prepend (parent->_children,
					     child);
	return TRUE;
}


static void
container_promote_child (Container *c, Container *child)
{
	GSList *iter;
	
	g_return_if_fail (c != child);
	g_return_if_fail (child);
	
	for (iter = child->_children; iter; iter = g_slist_next(iter)) 
		/* reparent grandchildren */
		((Container*)iter->data)->_parent = c;		

	c->_children = g_slist_concat (c->_children,
				       child->_children);
	child->_children = NULL;
}


static void
container_remove_child (Container *c, Container *child)
{
	g_return_if_fail (c != child);
	g_return_if_fail (child);

	c->_children = g_slist_remove (c->_children, child);
}


G_GNUC_UNUSED static void
container_dump (Container *c)
{
	const char* state;
	switch (c->_state) {
	case NUKE: state = "NUKE"; break;
	case SPLICE: state = "SPLICE"; break;
	case OKAY: state = "OKAY"; break;
	default: state = "HUH"; break;
	};
	
	g_print ("[%s] { %p parent=%p msg=%p [%s] children: %d state: %s}\n",
		 c->_msg ? mu_msg_get_subject(c->_msg) : "<empty>",
		 (void*)c,
		 (void*)c->_parent, (void*)c->_msg,
		 c->_msg ? mu_msg_get_msgid(c->_msg) : "",
		 g_slist_length (c->_children), state);
}


