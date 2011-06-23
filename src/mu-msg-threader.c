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
#include <math.h> /* for log, ceil */

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
 * the padding-0's are added to make them easy to sort using strcmp;
 * the number hexadecimal numbers, and the length of the 'segments'
 * (the parts separated by the ':') is equal to ceil(log_16(matchnum))
 * 
 */


/* Container data structure, as seen in the JWZ-doc; one differences
 * is that I use GSLists for the children, rather than 'next'
 * pointers
 *
 * */
struct _Container {
	MuMsg			*_msg;
	unsigned int		_docid;
	struct _Container	*_parent;
	GSList			*_children;
	gboolean		_dup;
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
static void container_splice_child (Container *c, Container *child);
static gboolean container_is_root (Container *c);
static void container_dump_tree (Container *c);
static void container_remove_child (Container *c, Container *child);

/* step 1 */ static GHashTable* create_containers (MuMsgIter *iter);
/* step 2 */ static Container *find_root (GHashTable *ids);
static void prune_empty_containers (Container *root);
/* static void group_root_set_by_subject (GSList *root_set); */
GHashTable* create_doc_id_thread_path_hash (Container *root, size_t match_num);
static void sort_by_date (Container *root);

/* msg threading algorithm, based on JWZ's algorithm,
 * http://www.jwz.org/doc/threading.html */
GHashTable*
mu_msg_threader_calculate (MuMsgIter *iter, size_t matchnum)
{
	GHashTable *id_table, *thread_ids;
	Container *root;

	g_return_val_if_fail (iter, FALSE);

	/* step 1 */
	id_table = create_containers (iter);
	
	/* step 2 -- JWZ calls this the 'root-set'; in our case, the
	 * root_set is the list of children for the dummy
	 * root-container */
	root = find_root (id_table);
	/* step 3: skip until the end; we still need to containers */
	//container_dump_tree (root);
	
	/* step 4: prune empty containers */
	prune_empty_containers (root);

	/* recalculate root set */
	sort_by_date (root);

	//container_dump_tree (root);

	
	/* step 5: group root set by subject */
	//group_root_set_by_subject (root_set);

	/* sort */
	mu_msg_iter_reset (iter); /* go all the way back */
	
	/* finally, deliver the docid => thread-path hash */
	thread_ids =  create_doc_id_thread_path_hash (root, matchnum);
	g_hash_table_destroy (id_table); /* step 3*/

	container_destroy (root);
	
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
		if (docid != 0)
			g_print ("*1 %s => %u\n", msgid, docid);
	} else if (c->_docid == 0) {
		c->_docid = docid;
		g_print ("*2 %s => %u\n", msgid, docid);
	} else if (docid != 0) { /* duplicate message-id */
		/* Container *c2; */
		/* char *fake_msgid; */
		/* g_print ("duplicate message-id %s\n", msgid); /\* FIXME: leak *\/ */
		/* c2 = container_new (NULL, docid); */
		/* c2->_parent = c; /\* make it a child of the other one...*\/ */
		/* c2->_dup = TRUE; */
		/* fake_msgid = g_strdup_printf ("%s_%u", msgid, docid); */
		/* g_hash_table_insert (id_table, fake_msgid, c2); */
		/* g_print ("*3 %s => %u\n", fake_msgid, docid); */
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
	
	/* now cur points to the final ref, which refers to our direct
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
		msg   = mu_msg_iter_get_msg (iter, NULL);
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
filter_root_set (const gchar *msgid, Container *c, Container *root)
{
	if (!c->_parent) /* this *before* adding it to the dummy root */
		container_add_child (root, c);
}


/* 2. Find the root - this is dummy container which takes the
 * until-now parentless Container-objects as children. JWZ calls this
 * the 'root_set'

   Walk over the elements of id_table, and gather a list of the
   Container objects that have no parents, but do have children */
static Container*
find_root (GHashTable *ids)
{
	GSList *lst;
	Container *root;
	
	root = container_new (NULL, 0);
		
	lst = NULL;
	g_hash_table_foreach (ids, (GHFunc)filter_root_set, root);
	
	return root;
}

/* this function will mark 'containers', and the do the pruning on
 * their children */
static void 
prune_empty_containers (Container *container)
{
	GSList *cur;
	
	for (cur = container->_children; cur; cur = g_slist_next (cur)) {
		
		Container *c;
		c = (Container*)cur->data;
		
		prune_empty_containers (c); /* recurse! */
				
		/* don't touch containers with messages */
		if (c->_msg)
			continue; 
	
		/* A. If it is an msg-less container with no children, nuke it. */
		if (!c->_children) {
			container_remove_child (c->_parent, c);
			continue;
		}		
		/* B. If the Container has no Message, but does have
		 * children, remove this container but promote its
		 * children to this level (that is, splice them in to
		 * the current child list.)
		 *
		 * Do not promote the children if doing so would
		 * promote them to the root set -- unless there is
		 * only one child, in which case, do.
		 */
		if (container_is_root(container) &&
		    g_slist_length(c->_children) != 1)
			continue;

		container_splice_child (container, c);
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


static gint
cmp_dates (Container *c1, Container *c2)
{
	MuMsg *m1, *m2;
	m1 = c1->_msg;
	m2 = c2->_msg;

	if (!m1)
		return m2 ?  1 : 0;
	if (!m2)
		return m1 ?  0 : 1; 
	
	return mu_msg_get_date (m1) - mu_msg_get_date (m2);
}

static void
sort_by_date (Container *container)
{
	GSList *cur;

	for (cur = container->_children; cur; cur = g_slist_next(cur)) {
		Container *c;
		c = (Container*)cur->data;
		sort_by_date (c); /* recurse */
	}

	container->_children = g_slist_sort (container->_children,
					     (GCompareFunc)cmp_dates);
}

static MuMsgIterThreadInfo*
thread_info_new (gchar *threadpath, gboolean root,
		 gboolean first_child, gboolean empty_parent, gboolean is_dup)
{
	MuMsgIterThreadInfo *ti;
	
	ti		     = g_slice_new (MuMsgIterThreadInfo);
	ti->threadpath	     = threadpath;

	ti->prop  = 0;
	ti->prop |= root         ? MU_MSG_ITER_THREAD_PROP_ROOT : 0;
	ti->prop |= first_child  ? MU_MSG_ITER_THREAD_PROP_FIRST_CHILD  : 0;
	ti->prop |= empty_parent ? MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT : 0;
	ti->prop |= is_dup       ? MU_MSG_ITER_THREAD_PROP_DUP : 0;

	if (is_dup)
		g_print ("dup: %s\n", threadpath);
	
	return ti;
}

static void
thread_info_destroy (MuMsgIterThreadInfo *ti)
{
	if (ti) {
		g_free (ti->threadpath);
		g_slice_free (MuMsgIterThreadInfo, ti);
	}
}


struct _ThreadInfo {
	GHashTable		*hash;
	GQueue			*idqueue;
	unsigned		 prev_level;
	const char*		 format;
};
typedef struct _ThreadInfo	 ThreadInfo;


struct _TP {
	char *threadpath;
	const char *frmt;
};
typedef struct _TP TP;
	

static void
accumulate_path (int seq, TP *tp)
{
	char segm[16];
	snprintf (segm, sizeof(segm), tp->frmt, seq);
		
	if (tp->threadpath) {
		char *path;
		path = g_strdup_printf ("%s:%s", tp->threadpath, segm);
		g_free (tp->threadpath);
		tp->threadpath = path;
	} else 
		tp->threadpath = g_strdup (segm);
}

static void
add_to_thread_info_hash (GHashTable *thread_info_hash, Container *c,
			 char *threadpath)
{
	gboolean is_root, first_child, empty_parent;
	
	/* 'root' means we're a child of the dummy root-container */
	is_root = container_is_root (c);
	
	first_child  = is_root ? FALSE : (c->_parent->_children->data == c);
	empty_parent = is_root ? FALSE : (!c->_parent->_msg); 
	
	g_hash_table_insert (thread_info_hash,
			     GUINT_TO_POINTER(c->_docid),
			     thread_info_new (threadpath,
					      container_is_root (c),
					      first_child,
					      empty_parent,
					      c->_dup));
}

/* device a format string that is the minimum size to fit up to
 * matchnum matches -- returns static memory */
const char*
thread_segment_format_string (size_t matchnum)
{
	unsigned digitnum;
	static char frmt[16];
	
	/* get the number of digits needed in a hex-representation of
	 * matchnum */
	digitnum = (unsigned) (ceil (log(matchnum)/log(16)));
	snprintf (frmt, sizeof(frmt),"%%0%ux", digitnum);
	
	return frmt;
}

static gboolean
add_thread_info (Container *c, guint level, ThreadInfo *ti)
{
	TP tp;
	unsigned i;

	/* ignore our dummy root container */
	if (!c->_parent)
		return TRUE;
	
	if (level > ti->prev_level) {
		for (i = ti->prev_level; i != level; ++i)
			g_queue_push_tail (ti->idqueue, GUINT_TO_POINTER(0));
	} else if (level <= ti->prev_level) {
		int oldseq;
		for (i = level; i < ti->prev_level; ++i)
			g_queue_pop_tail (ti->idqueue);

		if (g_queue_is_empty (ti->idqueue))
			g_queue_push_tail(ti->idqueue, GUINT_TO_POINTER(0));
		else {	
			oldseq = GPOINTER_TO_UINT(g_queue_pop_tail(ti->idqueue));
			g_queue_push_tail(ti->idqueue, GUINT_TO_POINTER(1 + oldseq));
		}
	}
	
	ti->prev_level = level;
	
	if (!c->_docid) 
		return TRUE; /* nothing more to do - it's a 'virtual'
			      * message */
			
	tp.threadpath = NULL;
	tp.frmt	      = ti->format;

	g_queue_foreach (ti->idqueue, (GFunc)accumulate_path, &tp);

	add_to_thread_info_hash (ti->hash, c, tp.threadpath);

	return TRUE;
}


GHashTable*
create_doc_id_thread_path_hash (Container *root, size_t matchnum)
{
	ThreadInfo ti;
	
	/* create hash docid => thread-path */
	ti.hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
					 NULL,
					 (GDestroyNotify)thread_info_destroy);
	ti.idqueue    = g_queue_new ();

	ti.prev_level = 0;
	ti.format     = thread_segment_format_string (matchnum);

	container_traverse (root, 0, (ContainerTraverseFunc)add_thread_info,
			    &ti);

	g_queue_free (ti.idqueue);
	return ti.hash;
}


static Container*
container_new (MuMsg *msg, unsigned docid)
{
	Container *c;

	c = g_slice_new (Container);

	c->_msg	      = msg ? mu_msg_ref (msg) : NULL;
	c->_docid     = docid;
	c->_dup       = FALSE;
	c->_children  = NULL;
	c->_parent    = NULL;

	
	return c;
}

static void
container_destroy (Container *c)
{
	if (!c)
		return;

	if (c->_msg)
		mu_msg_unref (c->_msg);

	c->_parent = (void*)0xdeadbeef;
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
	    already_referenced(parent, child)) {
		/* g_print ("already ref'd\n"); */
		return FALSE;
	}
	
	child->_parent	  = parent;
	parent->_children = g_slist_prepend (parent->_children,
					     child);
	
	return TRUE;
}


static void
container_splice_child (Container *c, Container *child)
{
	GSList *iter;
	
	g_return_if_fail (c != child);
	g_return_if_fail (child);
	g_return_if_fail (!child->_msg);
	
	for (iter = child->_children; iter; iter = g_slist_next(iter))
		((Container*)iter->data)->_parent = c;	/* reparent
							 * grandchildren */
	/*
	 * remove the old child
	 */
	c->_children = g_slist_remove (c->_children, child);
	/*
	 * put child's children first, so we can use this while
	 * iterating over c->_children (we are already past the merged
	 * child's children)
	 */
	c->_children = g_slist_concat (child->_children, c->_children);
	
	child->_children = NULL;
	child->_parent   = NULL;
}


static void
container_remove_child (Container *c, Container *child)
{
	g_return_if_fail (c != child);
	g_return_if_fail (child);
	g_return_if_fail (!child->_children);
	
	c->_children = g_slist_remove (c->_children, child);
}
	

static gboolean
container_is_root (Container *c)
{
	return (!c->_parent || !c->_parent->_parent);
}

G_GNUC_UNUSED static void
container_dump (Container *c)
{	
	g_print ("[%s] { %p parent=%p msg=%p docid=%u [%s] children: %d }\n",
		 c->_msg ? mu_msg_get_subject(c->_msg) : "<empty>",
		 (void*)c,
		 (void*)c->_parent, (void*)c->_msg,
		 c->_docid,
		 c->_msg ? mu_msg_get_msgid(c->_msg) : "",
		 g_slist_length (c->_children));
}


static gboolean
each_container (Container *c, guint level)
{
	while (level--) 
		fputs ("  ", stdout);
		
	container_dump (c);

	return TRUE;
}



G_GNUC_UNUSED static void
container_dump_tree (Container *c)
{	
	container_traverse (c, 0, (ContainerTraverseFunc)each_container, NULL);
}


