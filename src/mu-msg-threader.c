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
#include <string.h> /* for memset */

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

/*
 * path data structure, to determine the thread paths mentioned
 * above
 * */ 
struct _Path {
	int *_data;
	guint _len;
};
typedef struct _Path Path;

static Path* path_new (guint initial);
static void  path_destroy (Path *p);
static void  path_inc (Path *p, guint index);
static gchar* path_to_string (Path *p, const char* frmt);



/* Container data structure, as seen in the JWZ-doc*
 *
*/
enum _ContainerFlag {
	CONTAINER_FLAG_NONE    = 0,
	CONTAINER_FLAG_DELETE  = 1 << 0,
	CONTAINER_FLAG_SPLICE  = 1 << 1,
	CONTAINER_FLAG_DUP     = 1 << 2
};
typedef guint8 ContainerFlag;

struct _Container {
	struct _Container *parent, *child, *next;
	ContainerFlag flags;
	MuMsg *msg;
	guint docid;
	const char* msgid;
};
typedef struct _Container Container;

static Container* container_new (MuMsg *msg, guint docid, const char* msgid);
static void       container_destroy (Container *c);
static void       container_add_child (Container *c, Container *child);
static void       container_add_sibling (Container *c, Container *sibling);
static void       container_remove_child (Container *c, Container *child);

typedef gboolean (*ContainerForeachFunc) (Container*, gpointer);
static gboolean   container_foreach (Container *c,
				     ContainerForeachFunc func,
				     gpointer user_data);

typedef void (*ContainerPathForeachFunc) (Container*, gpointer, Path*);
static  void   container_path_foreach (Container *c,
					  ContainerPathForeachFunc func,
					  gpointer user_data);

static void       container_splice (Container *parent, Container *child);
size_t            container_child_count (Container *c);
static gboolean   container_reachable (Container *haystack, Container *needle);
static void       container_dump (Container *c, gboolean recursive);






/* step 1 */ static GHashTable* create_containers (MuMsgIter *iter);
/* step 2 */ static Container *find_root_set (GHashTable *ids);
static void prune_empty_containers (Container *root);
/* static void group_root_set_by_subject (GSList *root_set); */
GHashTable* create_doc_id_thread_path_hash (Container *root, size_t match_num);
static Container* sort_by_date (Container *root);

/* msg threading algorithm, based on JWZ's algorithm,
 * http://www.jwz.org/doc/threading.html */
GHashTable*
mu_msg_threader_calculate (MuMsgIter *iter, size_t matchnum)
{
	GHashTable *id_table, *thread_ids;
	Container *root_set;

	g_return_val_if_fail (iter, FALSE);

	/* step 1 */
	id_table = create_containers (iter);
	
	/* step 2 -- the root_set is the list of children without parent */
	root_set = find_root_set (id_table);

	/* step 3: skip until the end; we still need to containers */

	/* step 4: prune empty containers */
	prune_empty_containers (root_set);
	
	/* recalculate root set */
	root_set = sort_by_date (root_set);
	
	/* step 5: group root set by subject */
	//group_root_set_by_subject (root_set);

	/* sort */
	mu_msg_iter_reset (iter); /* go all the way back */
	
	/* finally, deliver the docid => thread-path hash */
	thread_ids =  create_doc_id_thread_path_hash (root_set,
						      matchnum);
	g_hash_table_destroy (id_table); /* step 3*/

	return thread_ids;
}

/* a referred message is a message that is refered by some other message */
static Container*
find_or_create_referred (GHashTable *id_table, const char *msgid)
{
	Container *c;

	g_return_val_if_fail (msgid, NULL);

	c = g_hash_table_lookup (id_table, msgid);	
	if (!c) {
		c = container_new (NULL, 0, msgid);
		g_hash_table_insert (id_table, (gpointer)msgid, c);
	}

	return c;
}

/* find a container for the given msgid; if it does not exist yet,
 * create a new one, and register it */
static Container*
find_or_create (GHashTable *id_table, MuMsg *msg, guint docid)
{
	Container *c;
	const char* msgid;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (docid != 0, NULL);
	
	msgid = mu_msg_get_msgid (msg);
	if (!msgid)
		msgid = mu_msg_get_path (msg); /* fake it */
	
	c = g_hash_table_lookup (id_table, msgid);	

	/* If id_table contains an empty Container for this ID: * *
	 * Store this message in the Container's message slot. */
	if (c && !c->msg) {
		c->msg = mu_msg_ref (msg);
		c->docid = docid;
		return c;
		
	} else if (!c) { /* Else: Create a new Container object holding
			  this message; Index the Container by
			  Message-ID in id_table. */
		c = container_new (msg, docid, msgid);
		g_hash_table_insert (id_table, (gpointer)msgid, c);
		return c;

	} else {  /* c && c->msg */
		/* special case, not in the JWZ algorithm: the
		 * container exists already and has a message; this
		 * means that we are seeing *another message* with a
		 * message-id we already saw... create this message,
		 * and mark it as a duplicate, and a child of the one
		 * we saw before; use its path as a fake message-id*/
		Container *c2;
		c2 = container_new (msg, docid, msgid);
		c2->flags = CONTAINER_FLAG_DUP;
		container_add_child (c, c2);
		g_hash_table_insert (id_table,
				     (gpointer)mu_msg_get_path (msg), c2);
		return NULL; /* don't process this message further */
	}
}

static void /* 1B */
handle_references (GHashTable *id_table, Container *c)
{
	const GSList *refs, *cur;
	Container *parent;
	
	refs = mu_msg_get_references (c->msg);
	if (!refs) 
		return; /* nothing to do */

	/* For each element in the message's References field:

	   Find a Container object for the given Message-ID: If
	   there's one in id_table use that; Otherwise, make (and
	   index) one with a null Message. */
		
	/* go over over our list of refs, until 1 before the last... */ 
	for (parent = NULL, cur = refs; cur; cur = g_slist_next (cur)) {
		
		Container *child;
		
		child = find_or_create_referred (id_table, (gchar*)cur->data);
	
		/*Link the References field's Containers together in
		 * the order implied by the References header.

		 If they are already linked, don't change the existing
		 links.  Do not add a link if adding that link would
		 introduce a loop: that is, before asserting A->B,
		 search down the children of B to see if A is
		 reachable, and also search down the children of A to
		 see if B is reachable. If either is already reachable
		 as a child of the other, don't add the link. */
		
		if (parent &&
		    !container_reachable (parent, child) &&
		    !container_reachable (child, parent)) {
			container_add_child (parent, child);
		}
		parent = child;
	}

	/* 'parent' points to the last ref: our direct parent; 
	   
	   Set the parent of this message to be the last element in
	   References. Note that this message may have a parent
	   already: this can happen because we saw this ID in a
	   References field, and presumed a parent based on the other
	   entries in that field. Now that we have the actual message,
	   we can be more definitive, so throw away the old parent and
	   use this new one. Find this Container in the parent's
	   children list, and unlink it.
	   
	   Note that this could cause this message to now have no
	   parent, if it has no references field, but some message
	   referred to it as the non-first element of its
	   references. (Which would have been some kind of lie...)
	   
	   Note that at all times, the various ``parent'' and ``child'' fields
	   must be kept inter-consistent. */
	if (!container_reachable (parent, c) &&
	    !container_reachable (c, parent)) {
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
					  NULL,
					  (GDestroyNotify)container_destroy);
	
	for (mu_msg_iter_reset (iter); !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next (iter)) {
		
		Container *c;
		MuMsg *msg;
		unsigned docid;
			
		/* 1.A */
		msg   = mu_msg_iter_get_msg (iter, NULL);
		docid = mu_msg_iter_get_docid (iter);
		
		c = find_or_create (id_table, msg, docid);

		/* 1.B and C */
		if (c)
			handle_references (id_table, c);		
	}	

	return id_table;
}



static void
filter_root_set (const gchar *msgid, Container *c, Container **root_set)
{
	if (c->parent)
		return; 
	
	if (*root_set == NULL)
		*root_set = c;
	else
		container_add_sibling (*root_set, c);
}


/* 2.  Walk over the elements of id_table, and gather a list of the
   Container objects that have no parents, but do have children */
static Container*
find_root_set (GHashTable *ids)
{
	Container *root_set;
		
	root_set = NULL;
	g_hash_table_foreach (ids, (GHFunc)filter_root_set, &root_set);

	return root_set;
}


static gboolean
prune_maybe (Container *c)
{
	Container *cur;
	
	for (cur = c->child; cur; cur = cur->next) {
		if (cur->flags & CONTAINER_FLAG_DELETE) 
			container_remove_child (c, cur);
		else if (cur->flags & CONTAINER_FLAG_SPLICE)
			container_splice (c, cur);
	}
	
	/* don't touch containers with messages */
	if (c->msg) 
		return TRUE;
	
	/* A. If it is an msg-less container with no children, mark it for deletion. */
	if (!c->child) {
		c->flags |= CONTAINER_FLAG_DELETE;
		return TRUE;
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
	if (!c->parent || (!c->parent->parent && container_child_count(c) != 1))
		return TRUE;

	c->flags |= CONTAINER_FLAG_SPLICE;
	
	return TRUE;
}



static void 
prune_empty_containers (Container *container)
{
	Container *dummy, *cur;

	dummy = container_new (NULL, 0, "DUMMY");

	container_add_child (dummy, container);
	
	container_foreach (dummy, (ContainerForeachFunc)prune_maybe, NULL);
	
	for (cur = dummy->child; cur; cur = cur->next)
		cur->parent = NULL;

	container_destroy (dummy);
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

G_GNUC_UNUSED static gint
cmp_dates (Container *c1, Container *c2)
{
	MuMsg *m1, *m2;
	
	m1 = c1->msg;
	m2 = c2->msg;

	if (!m1)
		return m2 ?  1 : 0;
	if (!m2)
		return m1 ?  0 : 1;
	
	return mu_msg_get_date (m1) - mu_msg_get_date (m2);
}


/* yuck ugly */
G_GNUC_UNUSED static Container*
sort_by_date (Container *c)
{
	GSList *lst, *cur;

	if (!c)
		return NULL;

	c->child = sort_by_date (c->child); /* recurse! */
	
	for (lst = NULL; c; c = c->next) {
		lst = g_slist_prepend (lst, c);
	}
		
	lst = g_slist_sort (lst, (GCompareFunc)cmp_dates);

	c = (Container*)lst->data;
	c->next = NULL;
	cur = g_slist_next (lst);
		
	for (;cur; cur = g_slist_next(cur)) {
		Container *sib;
		sib = (Container*)cur->data;
		sib->next = NULL;
		container_add_sibling (c, sib);	
	}
		
	g_slist_free (lst);
	return c;
}


static MuMsgIterThreadInfo*
thread_info_new (gchar *threadpath, gboolean root,
		 gboolean child, gboolean empty_parent, gboolean is_dup)
{
	MuMsgIterThreadInfo *ti;
	
	ti		     = g_slice_new (MuMsgIterThreadInfo);
	ti->threadpath	     = threadpath;

	ti->prop  = 0;
	ti->prop |= root         ? MU_MSG_ITER_THREAD_PROP_ROOT : 0;
	ti->prop |= child        ? MU_MSG_ITER_THREAD_PROP_FIRST_CHILD  : 0;
	ti->prop |= empty_parent ? MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT : 0;
	ti->prop |= is_dup       ? MU_MSG_ITER_THREAD_PROP_DUP : 0;
	
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
	const char*		 format;
};
typedef struct _ThreadInfo	 ThreadInfo;


static void
add_to_thread_info_hash (GHashTable *thread_info_hash, Container *c,
			 char *threadpath)
{
	gboolean is_root, child, empty_parent, is_dup;
	
	/* 'root' means we're a child of the dummy root-container */
	is_root = (c->parent == NULL);
	
	child	     = is_root ? FALSE : (c->parent->child == c);
	empty_parent = is_root ? FALSE : (!c->parent->msg); 
	is_dup	     = c->flags &	CONTAINER_FLAG_DUP;
	
	g_hash_table_insert (thread_info_hash,
			     GUINT_TO_POINTER(c->docid),
			     thread_info_new (threadpath,
					      is_root,
					      child,
					      empty_parent,
					      is_dup));
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
add_thread_info (Container *c, ThreadInfo *ti, Path *path)
{	
	gchar *pathstr;

	pathstr = path_to_string (path, ti->format);
	
	add_to_thread_info_hash (ti->hash, c, pathstr);

	return TRUE;
}


GHashTable*
create_doc_id_thread_path_hash (Container *root_set, size_t matchnum)
{
	ThreadInfo ti;
	
	/* create hash docid => thread-info */
	ti.hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
					 NULL,
					 (GDestroyNotify)thread_info_destroy);

	ti.format     = thread_segment_format_string (matchnum);
	
	container_path_foreach (root_set,
				(ContainerPathForeachFunc)add_thread_info,
				&ti);

	return ti.hash;
}


static Container*
container_new (MuMsg *msg, guint docid, const char *msgid)
{
	Container *c;

	g_return_val_if_fail (!msg || docid != 0, NULL);
	
	c = g_slice_new0 (Container);
	
	if (msg)
		c->msg = mu_msg_ref (msg);


	
	c->docid = docid;
	c->msgid = msgid;
	
	return c;
}

static void
container_destroy (Container *c)
{
	if (!c)
		return;

	if (c->msg)
		mu_msg_unref (c->msg);

	g_slice_free (Container, c);
}


static void
container_add_sibling (Container *c, Container *sibling)
{
	Container *cur;

	g_return_if_fail (c);
	g_return_if_fail (sibling);
	g_return_if_fail (c != sibling);
	
	for (cur = sibling; cur; cur = cur->next)
		cur->parent = c->parent;

	for (cur = c; cur->next; cur = cur->next);
	cur->next = sibling;
}


static void
container_add_child (Container *c, Container *child)
{
	Container *cur;
	
	g_return_if_fail (c);
	g_return_if_fail (child);
	g_return_if_fail (c != child);
		
	for (cur = child; cur; cur = cur->next)
		cur->parent = c;

	if (!c->child)
		c->child = child;
	else {
		for (cur = c->child; cur->next; cur = cur->next);
		cur->next = child;
	}
}


static void
container_remove_child (Container *c, Container *child)
{
	Container *cur, *prev;

	g_return_if_fail (c);
	g_return_if_fail (child);
	g_return_if_fail (!child->child);
	g_return_if_fail (c != child);

	/* g_print ("%s: %s <-- %s\n", __FUNCTION__, c->msgid, */
	/* 	 child->msgid); */
	
	for (prev = NULL, cur = c->child; cur; cur = cur->next) {
		
		if (cur == child) {
			if (!prev)
				c->child = cur->next;
			else
				prev->next = cur->next;
		}
		prev = cur;		
	}
}

static void
container_path_foreach_real (Container *c, guint level, Path *path,
			     ContainerPathForeachFunc func, gpointer user_data)
{
	if (!c)
		return;

	path_inc (path, level);
	func (c, user_data, path);
	
	/* children */
	container_path_foreach_real (c->child, level + 1, path, func, user_data);

	/* siblings */
	container_path_foreach_real (c->next, level, path, func, user_data);
}


static void
container_path_foreach (Container *c, ContainerPathForeachFunc func,
			gpointer user_data)
{
	Path *path;
		
	path = path_new (100);
	
	container_path_foreach_real (c, 0, path, func, user_data);

	path_destroy (path);
}


static gboolean
container_foreach (Container *c, ContainerForeachFunc func, gpointer user_data)
{
	if (!c)
		return TRUE;
	
	if (!container_foreach (c->child, func, user_data))
		return FALSE; /* recurse into children */
	
	/* recurse into siblings */		
	if (!container_foreach (c->next, func, user_data))
		return FALSE;

	return func (c, user_data);
}


static void
container_splice (Container *parent, Container *child)
{
	g_return_if_fail (parent);
	g_return_if_fail (child);
	g_return_if_fail (parent != child);

	/* g_print ("%s: %s <-- %s\n", __FUNCTION__, parent->msgid, */
	/* 	 child->msgid); */
	
	container_add_child (parent, child->child);
	child->child = NULL;
	container_remove_child (parent, child);
}

size_t
container_child_count (Container *c)
{
	size_t count;
	Container *cur;
	
	g_return_val_if_fail (c, 0);

	for (count = 0, cur = c->child; cur; cur = cur->next)
		++count;

	return count;
}


static gboolean
different_container (Container *a, Container *b)
{
	/* level == 0 so we don't compare with ourselves... */
	return a != b;
}


static gboolean
container_reachable (Container *haystack, Container *needle)
{
	return container_foreach (haystack,
				  (ContainerForeachFunc)different_container,
				  needle) ? FALSE : TRUE;
}

static gboolean
dump_container (Container *c)
{
	const gchar* subject;
	
	if (!c) {
		g_print ("<empty>\n");
		return TRUE;
	}
	
	subject =  (c->msg) ? mu_msg_get_subject (c->msg) : "<none>";
	
	g_print ("[%s][%s m:%p p:%p docid:%u]\n",c->msgid, subject, (void*)c,
		 (void*)c->parent, c->docid);

	return TRUE;
}


G_GNUC_UNUSED static void
container_dump (Container *c, gboolean recursive)
{
	if (!recursive)
		dump_container (c);
	else
		container_foreach (c, (ContainerForeachFunc)dump_container,
				   NULL);
}


static Path*
path_new (guint initial)
{
	Path *p;

	p = g_slice_new0 (Path);
	
	p->_data = g_new0 (int, initial);
	p->_len  = initial;
	
	return p;
}

static void
path_destroy (Path *p)
{
	if (!p)
		return;
	
	g_free (p->_data);
	g_slice_free (Path, p);
}

static void
path_inc (Path *p, guint index)
{
	if (index + 1 >= p->_len) {
		p->_data = g_renew (int, p->_data, 2 * p->_len);
		memset (&p->_data[p->_len], 0, p->_len);
		p->_len *= 2;
	}
	
	++p->_data[index];
	p->_data[index + 1] = 0;
}

static gchar*
path_to_string (Path *p, const char* frmt)
{
	char *str;
	guint u;

	if (!p->_data)
		return NULL;
	
	for (u = 0, str = NULL; p->_data[u] != 0; ++u) {

		char segm[16];
		snprintf (segm, sizeof(segm), frmt, p->_data[u] - 1);
		
		if (!str) 
			str = g_strdup (segm);
		else {
			gchar *tmp;
			tmp = g_strdup_printf ("%s:%s", str, segm);
			g_free (str);
			str = tmp;
		}
	}
	
	return str;
}
