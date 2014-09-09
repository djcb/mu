/*
** Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string.h> /* for memset */
#include <math.h> /* for log, ceil */

#include "mu-container.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"


/*
 * path data structure, to determine the thread paths mentioned above;
 * the path is filled as we're traversing the tree of MuContainers
 * (messages)
 */
struct _Path {
	int *_data;
	guint _len;
};
typedef struct _Path Path;

static Path* path_new (guint initial);
static void  path_destroy (Path *p);
static void  path_inc (Path *p, guint index);
static gchar* path_to_string (Path *p, const char* frmt);

MuContainer*
mu_container_new (MuMsg *msg, guint docid, const char *msgid)
{
	MuContainer *c;

	g_return_val_if_fail (!msg || docid != 0, NULL);

	c = g_slice_new0 (MuContainer);
	if (msg)
		c->msg = mu_msg_ref (msg);

	c->leader = c;
	c->docid = docid;
	c->msgid = msgid;

	return c;
}

void
mu_container_destroy (MuContainer *c)
{
	if (!c)
		return;

	if (c->msg)
		mu_msg_unref (c->msg);

	g_slice_free (MuContainer, c);
}


static void
set_parent (MuContainer *c, MuContainer *parent)
{
	while (c) {
		c->parent = parent;
		c = c->next;
	}
}


G_GNUC_UNUSED static gboolean
check_dup (MuContainer *c, GHashTable *hash)
{
	if (g_hash_table_lookup (hash, c)) {
		g_warning ("ALREADY!!");
		mu_container_dump (c, TRUE);
		g_assert (0);
	} else
		g_hash_table_insert (hash, c, GUINT_TO_POINTER(TRUE));

	return TRUE;
}


G_GNUC_UNUSED static void
assert_no_duplicates (MuContainer *c)
{
	GHashTable *hash;

	hash = g_hash_table_new (g_direct_hash, g_direct_equal);

	mu_container_foreach (c,
			   (MuContainerForeachFunc)check_dup,
			   hash);

	g_hash_table_destroy (hash);
}


MuContainer*
mu_container_append_siblings (MuContainer *c, MuContainer *sibling)
{
	g_assert (c);

	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (sibling, NULL);
	g_return_val_if_fail (c != sibling, NULL);

	/* assert_no_duplicates (c); */

	set_parent (sibling, c->parent);

	/* find the last sibling and append; first we try our cache
	 * 'last', otherwise we need to walk the chain. We use a
	 * cached last as to avoid walking the chain (which is
	 * O(n*n)) */
	if (c->last)
		c->last->next = sibling;
	else {
		/* no 'last' cached, so walk the chain */
		MuContainer *c2;
		for (c2 = c; c2 && c2->next; c2 = c2->next);
		c2->next = sibling;
	}
	/* update the cached last */
	c->last = sibling->last ? sibling->last : sibling;

	/* assert_no_duplicates (c); */

	return c;
}

MuContainer*
mu_container_remove_sibling (MuContainer *c, MuContainer *sibling)
{
	MuContainer *cur, *prev;

	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (sibling, NULL);

	for (prev = NULL, cur = c; cur; cur = cur->next) {

		if (cur == sibling) {
			if (!prev)
				c = cur->next;
			else
				prev->next = cur->next;
			break;
		}
		prev = cur;
	}

	/* unset the cached last; it's not valid anymore
	 *
	 * TODO: we could actually do a better job updating last
	 * rather than invalidating it. */
	if (c)
		c->last = NULL;

	return c;
}

MuContainer*
mu_container_append_children (MuContainer *c, MuContainer *child)
{
	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (child, NULL);
	g_return_val_if_fail (c != child, NULL);

	/* assert_no_duplicates (c); */

	set_parent (child, c);
	if (!c->child)
		c->child = child;
	else
		c->child = mu_container_append_siblings (c->child, child);

	/* assert_no_duplicates (c->child); */

	return c;
}


MuContainer*
mu_container_remove_child (MuContainer *c, MuContainer *child)
{
	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (child, NULL);

	/* g_assert (!child->child); */
	/* g_return_val_if_fail (!child->child, NULL); */
	g_return_val_if_fail (c != child, NULL);

	c->child = mu_container_remove_sibling (c->child, child);

	return c;
}

typedef void (*MuContainerPathForeachFunc) (MuContainer*, gpointer, Path*);

static void
mu_container_path_foreach_real (MuContainer *c, guint level, Path *path,
				MuContainerPathForeachFunc func,
				gpointer user_data)
{
	if (!c)
		return;

	path_inc (path, level);
	func (c, user_data, path);

	/* children */
	mu_container_path_foreach_real (c->child, level + 1, path,
					func, user_data);

	/* siblings */
	mu_container_path_foreach_real (c->next, level, path, func, user_data);
}

static void
mu_container_path_foreach (MuContainer *c, MuContainerPathForeachFunc func,
			gpointer user_data)
{
	Path *path;

	path = path_new (100);

	mu_container_path_foreach_real (c, 0, path, func, user_data);

	path_destroy (path);
}


gboolean
mu_container_foreach (MuContainer *c, MuContainerForeachFunc func,
		      gpointer user_data)
{
	g_return_val_if_fail (func, FALSE);

	if (!c)
		return TRUE;

	if (!mu_container_foreach (c->child, func, user_data))
		return FALSE; /* recurse into children */

	/* recurse into siblings */
	if (!mu_container_foreach (c->next, func, user_data))
		return FALSE;

	return func (c, user_data);
}

MuContainer*
mu_container_splice_children (MuContainer *c, MuContainer *sibling)
{
	MuContainer *children;

	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (sibling, NULL);

	children = sibling->child;
	sibling->child = NULL;

	return mu_container_append_siblings (c, children);
}

MuContainer*
mu_container_splice_grandchildren (MuContainer *parent, MuContainer *child)
{
	MuContainer *newchild;

	g_return_val_if_fail (parent, NULL);
	g_return_val_if_fail (child, NULL);
	g_return_val_if_fail (parent != child, NULL);

	newchild = child->child;
	child->child=NULL;

	return mu_container_append_children (parent, newchild);
}


static GSList*
mu_container_to_list (MuContainer *c)
{
	GSList *lst;

	for (lst = NULL; c; c = c->next)
		lst = g_slist_prepend (lst, c);

	return lst;
}

static gpointer
list_last_data (GSList *lst)
{
	GSList *tail;

	tail = g_slist_last (lst);

	return tail->data;
}

static MuContainer*
mu_container_from_list (GSList *lst)
{
	MuContainer *c, *cur, *tail;

	if (!lst)
		return NULL;

	tail = list_last_data (lst);
	for (c = cur = (MuContainer*)lst->data; cur; lst = g_slist_next(lst)) {
		cur->next = lst ? (MuContainer*)lst->data : NULL;
		cur->last = tail;
		cur=cur->next;
	}

	return c;
}

struct _SortFuncData {
	MuMsgFieldId         mfid;
	gboolean             descending;
	gpointer             user_data;
};
typedef struct _SortFuncData SortFuncData;

static int
container_cmp (MuContainer *a, MuContainer *b, MuMsgFieldId mfid)
{
	if (a == b)
		return 0;
	else if (!a->msg)
		return -1;
	else if (!b->msg)
		return 1;

	return mu_msg_cmp (a->msg, b->msg, mfid);
}

static gboolean
container_is_leaf (const MuContainer *c)
{
	return c->child == NULL;
}

static MuContainer*
container_max (MuContainer *a, MuContainer *b, MuMsgFieldId mfid)
{
	return container_cmp (a, b, mfid) > 0 ? a : b;
}

static MuContainer*
find_sorted_tree_leader (MuContainer *root, SortFuncData *order)
{
	MuContainer *last_child;

	if (container_is_leaf (root))
		return root;

	if (!order->descending)
		last_child = root->child->last;
	else /* reversed order, first is last */
		last_child = root->child;

	return container_max (root, last_child->leader, order->mfid);
}

static int
sort_func_wrapper (MuContainer *a, MuContainer *b, SortFuncData *data)
{
	if (data->descending)
		return container_cmp (b->leader, a->leader, data->mfid);
	else
		return container_cmp (a->leader, b->leader, data->mfid);
}

static MuContainer*
container_sort_real (MuContainer *c, SortFuncData *sfdata)
{
	GSList *lst;
	MuContainer *cur;

	if (!c)
		return NULL;

	for (cur = c; cur; cur = cur->next) {
		if (cur->child)
			cur->child = container_sort_real (cur->child, sfdata);
		cur->leader = find_sorted_tree_leader (cur, sfdata);
	}

	/* sort siblings */
	lst = mu_container_to_list (c);
	lst = g_slist_sort_with_data(lst,
				     (GCompareDataFunc)sort_func_wrapper,
				     sfdata);
	c = mu_container_from_list (lst);
	g_slist_free (lst);

	return c;
}

MuContainer*
mu_container_sort (MuContainer *c, MuMsgFieldId mfid, gboolean descending,
		   gpointer user_data)
{
	SortFuncData sfdata;

	sfdata.mfid	  = mfid;
	sfdata.descending = descending;
	sfdata.user_data  = user_data;

	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), NULL);

	return container_sort_real (c, &sfdata);
}


static gboolean
unequal (MuContainer *a, MuContainer *b)
{
	return a == b ? FALSE : TRUE;
}


gboolean
mu_container_reachable (MuContainer *haystack, MuContainer *needle)
{
	g_return_val_if_fail (haystack, FALSE);
	g_return_val_if_fail (needle, FALSE);

	if (!mu_container_foreach
	    (haystack, (MuContainerForeachFunc)unequal, needle))
		return TRUE;

	return FALSE;
}


static gboolean
dump_container (MuContainer *c)
{
	const gchar* subject;

	if (!c) {
		g_print ("<empty>\n");
		return TRUE;
	}

	subject =  (c->msg) ? mu_msg_get_subject (c->msg) : "<none>";

	g_print ("[%s][%s m:%p p:%p docid:%u %s]\n",c->msgid, subject, (void*)c,
		 (void*)c->parent, c->docid,
		 c->msg ? mu_msg_get_path (c->msg) : "");

	return TRUE;
}


void
mu_container_dump (MuContainer *c, gboolean recursive)
{
	g_return_if_fail (c);

	if (!recursive)
		dump_container (c);
	else
		mu_container_foreach
			(c,
			 (MuContainerForeachFunc)dump_container,
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

static unsigned
count_colons (const char *str)
{
	unsigned num;

	num = 0;
	while (str++ && *str)
		if (*str == ':')
			++num;

	return num;
}



static MuMsgIterThreadInfo*
thread_info_new (gchar *threadpath, gboolean root, gboolean first_child,
		 gboolean empty_parent, gboolean has_child, gboolean is_dup)
{
	MuMsgIterThreadInfo *ti;

	ti		     = g_slice_new (MuMsgIterThreadInfo);
	ti->threadpath	     = threadpath;
	ti->level            = count_colons (threadpath); /* hacky... */

	ti->prop  = MU_MSG_ITER_THREAD_PROP_NONE;
	ti->prop |= root         ? MU_MSG_ITER_THREAD_PROP_ROOT         : 0;
	ti->prop |= first_child  ? MU_MSG_ITER_THREAD_PROP_FIRST_CHILD  : 0;
	ti->prop |= empty_parent ? MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT : 0;
	ti->prop |= is_dup       ? MU_MSG_ITER_THREAD_PROP_DUP          : 0;
	ti->prop |= has_child    ? MU_MSG_ITER_THREAD_PROP_HAS_CHILD    : 0;

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
	GHashTable *hash;
	const char *format;
};
typedef struct _ThreadInfo ThreadInfo;


static void
add_to_thread_info_hash (GHashTable *thread_info_hash, MuContainer *c,
			 char *threadpath)
{
	gboolean is_root, first_child, empty_parent, is_dup, has_child;

	/* 'root' means we're a child of the dummy root-container */
	is_root = (c->parent == NULL);

	first_child  = is_root ? FALSE : (c->parent->child == c);
	empty_parent = is_root ? FALSE : (!c->parent->msg);
	is_dup	     = c->flags & MU_CONTAINER_FLAG_DUP;
	has_child    = c->child ? TRUE : FALSE;

	g_hash_table_insert (thread_info_hash,
			     GUINT_TO_POINTER(c->docid),
			     thread_info_new (threadpath,
					      is_root,
					      first_child,
					      empty_parent,
					      has_child,
					      is_dup));
}

/* device a format string that is the minimum size to fit up to
 * matchnum matches -- returns static memory */
static const char*
thread_segment_format_string (size_t matchnum)
{
	unsigned digitnum;
	static char frmt[16];

	/* get the number of digits needed in a hex-representation of
	 * matchnum */
	digitnum = (unsigned) (ceil (log(matchnum)/log(16)));
	snprintf (frmt, sizeof(frmt), "%%0%ux", digitnum);

	return frmt;
}

static gboolean
add_thread_info (MuContainer *c, ThreadInfo *ti, Path *path)
{
	gchar *pathstr;

	pathstr = path_to_string (path, ti->format);
	add_to_thread_info_hash (ti->hash, c, pathstr);

	return TRUE;
}


GHashTable*
mu_container_thread_info_hash_new (MuContainer *root_set, size_t matchnum)
{
	ThreadInfo ti;

	g_return_val_if_fail (root_set, NULL);
	g_return_val_if_fail (matchnum > 0, NULL);

	/* create hash docid => thread-info */
	ti.hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
					 NULL,
					 (GDestroyNotify)thread_info_destroy);

	ti.format     = thread_segment_format_string (matchnum);

	mu_container_path_foreach (root_set,
				(MuContainerPathForeachFunc)add_thread_info,
				&ti);

	return ti.hash;
}
