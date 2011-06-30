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

#include <string.h> /* for memset */

#include "mu-threader-utils.h"
#include "mu-msg.h"

struct _Path {
	int *_data;
	guint _len;
};

Container*
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

void
container_destroy (Container *c)
{
	if (!c)
		return;

	if (c->msg)
		mu_msg_unref (c->msg);

	g_slice_free (Container, c);
}


static void
set_parent (Container *c, Container *parent)
{
	while (c) {
		c->parent = parent;
		c = c->next;
	}
}

static Container*
find_last (Container *c)
{	
	while (c && c->next)
		c = c->next;

	return c;
}


static gboolean
check_dup (Container *c, GHashTable *hash)
{
	if (g_hash_table_lookup (hash, c)) {
		g_warning ("ALREADY!!");
		container_dump (c, TRUE);
		g_assert (0);
	} else
		g_hash_table_insert (hash, c, GUINT_TO_POINTER(TRUE));

	return TRUE;
}


G_GNUC_UNUSED static void
assert_no_duplicates (Container *c)
{
	GHashTable *hash;

	hash = g_hash_table_new (g_direct_hash, g_direct_equal);
	
	container_foreach (c,
			   (ContainerForeachFunc)check_dup,
			   hash);
			
	g_hash_table_destroy (hash);
}



Container*
container_append_siblings (Container *c, Container *sibling)
{
	g_assert (c);
	
	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (sibling, NULL);
	g_return_val_if_fail (c != sibling, NULL);

	/* assert_no_duplicates (c); */
	
	set_parent (sibling, c->parent);
	(find_last(c))->next = sibling;

	/* assert_no_duplicates (c); */
	
	return c;
}

Container*
container_remove_sibling (Container *c, Container *sibling)
{
	Container *cur, *prev;
	
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

	return c;
}
	
Container*
container_append_children (Container *c, Container *child)
{
	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (child, NULL);
	g_return_val_if_fail (c != child, NULL);

	/* assert_no_duplicates (c); */
	
	set_parent (child, c);
	if (!c->child)
		c->child = child;
	else
		c->child = container_append_siblings (c->child, child);

	/* assert_no_duplicates (c->child); */
	
	return c;
}


Container*
container_remove_child (Container *c, Container *child)
{
	g_return_val_if_fail (c, NULL);
	g_return_val_if_fail (child, NULL);

	g_assert (!child->child);
	g_return_val_if_fail (!child->child, NULL);
	g_return_val_if_fail (c != child, NULL);

	c->child = container_remove_sibling (c->child, child);

	return c;
}

void
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


void
container_path_foreach (Container *c, ContainerPathForeachFunc func,
			gpointer user_data)
{
	Path *path;
		
	path = path_new (100);
	
	container_path_foreach_real (c, 0, path, func, user_data);

	path_destroy (path);
}


gboolean
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


Container*
container_splice_children (Container *parent, Container *child)
{
	Container *newchild;

	g_return_val_if_fail (parent, NULL);
	g_return_val_if_fail (child, NULL);
	g_return_val_if_fail (parent != child, NULL);

	newchild = child->child;
	child->child=NULL;

	container_remove_child (parent, child);

	return container_append_children (parent, newchild);
}


GSList*
container_to_list (Container *c)
{
	GSList *lst;

	for (lst = NULL; c; c = c->next) 
		lst = g_slist_prepend (lst, c);
	
	return lst;
}


static Container*
container_from_list (GSList *lst)
{
	Container *c, *cur;

	if (!lst)
		return NULL;

	for (c = cur = (Container*)lst->data; cur; lst = g_slist_next(lst)) {
		cur->next = lst ? (Container*)lst->data : NULL;
		cur=cur->next;
	}
	
	return c;
}

struct _SortFuncData {
	GCompareDataFunc     func;
	gboolean             invert;
	gpointer             user_data;
};
typedef struct _SortFuncData SortFuncData;


static int
sort_func_wrapper (Container *a, Container *b, SortFuncData *data)
{
	Container *a1, *b1;

	/* use the first non-empty 'left child' message if this one
	 * is */ 
	for (a1 = a; a1->msg == NULL && a1->child != NULL; a1 = a1->child);
	for (b1 = b; b1->msg == NULL && b1->child != NULL; b1 = b1->child);
	
	if (data->invert)
		return data->func (b1, a1, data->user_data);
	else
		return data->func (a1, b1, data->user_data);	
}

static Container*
container_sort_real (Container *c, SortFuncData *sfdata)
{
	GSList *lst;
	Container *cur;
			
	if (!c)
		return NULL;

	for (cur = c; cur; cur = cur->next) 
		if (cur->child)
			cur->child = container_sort_real (cur->child, sfdata);
		
	/* sort siblings */
	lst = container_to_list (c);
	lst = g_slist_sort_with_data(lst,
				     (GCompareDataFunc)sort_func_wrapper,
				     sfdata); 
	c = container_from_list (lst);
	g_slist_free (lst);
	
	return c;
}


Container *
container_sort (Container *c, GCompareDataFunc func, gpointer user_data,
		gboolean invert)
{
	SortFuncData sfdata = { func, invert, user_data };
	
	return container_sort_real (c, &sfdata);
}	


static gboolean
unequal (Container *a, Container *b)
{
	return a == b ? FALSE : TRUE;
}


gboolean
container_reachable (Container *haystack, Container *needle)
{
	if (!container_foreach
	    (haystack, (ContainerForeachFunc)unequal, needle))
		return TRUE;
	
	return FALSE;
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
	
	g_print ("[%s][%s m:%p p:%p docid:%u %s]\n",c->msgid, subject, (void*)c,
		 (void*)c->parent, c->docid,
		 c->msg ? mu_msg_get_path (c->msg) : "");

	return TRUE;
}


void
container_dump (Container *c, gboolean recursive)
{
	if (!recursive)
		dump_container (c);
	else
		container_foreach (c, (ContainerForeachFunc)dump_container,
				   NULL);
}



Path*
path_new (guint initial)
{
	Path *p;

	p = g_slice_new0 (Path);
	
	p->_data = g_new0 (int, initial);
	p->_len  = initial;
	
	return p;
}

void
path_destroy (Path *p)
{
	if (!p)
		return;
	
	g_free (p->_data);
	g_slice_free (Path, p);
}

void
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


gchar*
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
