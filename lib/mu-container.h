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

#ifndef __MU_CONTAINER_H__
#define __MU_CONTAINER_H__

#include <glib.h>
#include <mu-msg.h>

enum _MuContainerFlag {
	MU_CONTAINER_FLAG_NONE    = 0,
	MU_CONTAINER_FLAG_DELETE  = 1 << 0,
	MU_CONTAINER_FLAG_SPLICE  = 1 << 1,
	MU_CONTAINER_FLAG_DUP     = 1 << 2
};
typedef enum _MuContainerFlag MuContainerFlag;

/*
 * MuContainer data structure, as seen in JWZs document:
 *     http://www.jwz.org/doc/threading.html
 */
struct _MuContainer {
	struct _MuContainer *parent, *child, *next;

	/* note: we cache the last of the string of next->next->...
	 * `mu_container_append_siblings' shows up high in the
	 * profiles since it needs to walk to the end, and this give
	 * O(n*n) behavior.
	 * */
	struct _MuContainer *last;

	/* Node in the subtree rooted at this node which comes first
	 * in the descending sort order, e.g. the latest message if
	 * sorting by date. We compare the leaders when ordering
	 * subtrees. */
	struct _MuContainer *leader;

	MuMsg               *msg;
	const char          *msgid;

	unsigned            docid;
	MuContainerFlag     flags;

};
typedef struct _MuContainer MuContainer;


/**
 * create a new Container object
 *
 * @param msg a MuMsg, or NULL; when it's NULL, docid should be 0
 * @param docid a Xapian docid, or 0
 * @param msgid a message id, or NULL
 *
 * @return a new Container instance, or NULL in case of error; free
 * with mu_container_destroy
 */
MuContainer* mu_container_new (MuMsg *msg, guint docid, const char* msgid);


/**
 * free a Container object
 *
 * @param c a Container object, or NULL
 */
void       mu_container_destroy (MuContainer *c);



/**
 * append new child(ren) to this container; the child(ren) container's
 * parent pointer will point to this one
 *
 * @param c a Container instance
 * @param child a child
 *
 * @return the Container instance with a child added
 */
MuContainer* mu_container_append_children (MuContainer *c, MuContainer *child);

/**
 * append a new sibling to this (list of) containers; all the siblings
 * will get the same parent that @c has
 *
 * @param c a container instance
 * @param sibling a sibling
 *
 * @return the container (list) with the sibling(s) appended
 */
MuContainer* mu_container_append_siblings (MuContainer *c, MuContainer *sibling);

/**
 * remove a _single_ child container from a container
 *
 * @param c a container instance
 * @param child the child container to remove
 *
 * @return the container with the child removed; if the container did
 * have this child, nothing changes
 */
MuContainer* mu_container_remove_child (MuContainer *c, MuContainer *child);

/**
 * remove a _single_ sibling container from a container
 *
 * @param c a container instance
 * @param sibling the sibling container to remove
 *
 * @return the container with the sibling removed; if the container did
 * have this sibling, nothing changes
 */
MuContainer* mu_container_remove_sibling (MuContainer *c, MuContainer *sibling);

/**
 * promote sibling's children to be this container's siblings
 *
 * @param c a container instance
 * @param sibling a sibling of this container
 *
 * @return the container with the sibling's children promoted
 */

MuContainer* mu_container_splice_children (MuContainer *c,
                                           MuContainer *sibling);

/**
 * promote child's children to be parent's children
 *
 * @param parent a container instance
 * @param child a child of this container
 *
 * @return the new container with it's children's children promoted
 */
MuContainer* mu_container_splice_grandchildren (MuContainer *parent,
                                                MuContainer *child);

typedef gboolean (*MuContainerForeachFunc) (MuContainer*, gpointer);

/**
 * execute some function on all siblings an children of some container
 * (recursively) until all children have been visited or the callback
 * function returns FALSE
 *
 * @param c a container
 * @param func a function to call for each container
 * @param user_data a pointer to pass to the callback function
 *
 * @return
 */
gboolean   mu_container_foreach (MuContainer *c,
				 MuContainerForeachFunc func,
				 gpointer user_data);

/**
 * check wither container needle is a child or sibling (recursively)
 * of container haystack
 *
 * @param haystack a container
 * @param needle a container
 *
 * @return TRUE if needle is reachable from haystack, FALSE otherwise
 */
gboolean   mu_container_reachable (MuContainer *haystack, MuContainer *needle);


/**
 * dump the container to stdout (for debugging)
 *
 * @param c a container
 * @param recursive whether to include siblings, children
 */
void       mu_container_dump (MuContainer *c, gboolean recursive);


typedef int (*MuContainerCmpFunc) (MuContainer *c1, MuContainer *c2,
				   gpointer user_data);

/**
 * sort the tree of MuContainers, recursively; ie. each of the list of
 * siblings (children) will be sorted according to @func; if the
 * container is empty, the first non-empty 'leftmost' child is used.
 *
 * @param c a container
 * @param mfid the field to sort by
 * @param revert if TRUE, revert the sorting order *
 * @param user_data a user pointer to pass to the sorting function
 *
 * @return a sorted container
 */
MuContainer* mu_container_sort (MuContainer *c, MuMsgFieldId mfid,
				gboolean revert,
				gpointer user_data);


/**
 * create a hashtable with maps document-ids to information about them,
 * ie. Xapian docid => MuMsgIterThreadInfo
 *
 * @param root_set the containers @param matchnum the number of
 * matches in the list (this is needed to determine the shortest
 * possible collation keys ('threadpaths') for the messages
 *
 * @return a hash; free with g_hash_table_destroy
 */
GHashTable* mu_container_thread_info_hash_new (MuContainer *root_set,
					       size_t matchnum);

#endif /*__MU_CONTAINER_H__*/
