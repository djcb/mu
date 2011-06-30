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

#ifndef __MU_THREADER_UTILS_H__
#define __MU_THREADER_UTILS_H__

#include <glib.h>
#include <mu-msg.h>

/*
 * path data structure, to determine the thread paths mentioned
 * above
 * */ 
struct _Path;
typedef struct _Path Path;

Path* path_new (guint initial);
void  path_destroy (Path *p);
void  path_inc (Path *p, guint index);
gchar* path_to_string (Path *p, const char* frmt);

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

Container* container_new (MuMsg *msg, guint docid, const char* msgid);
void       container_destroy (Container *c);
Container* container_append_children (Container *c, Container *child);
Container* container_append_siblings (Container *c, Container *sibling);
Container* container_remove_child (Container *c, Container *child);
Container* container_remove_sibling (Container *c, Container *sibling);
Container* container_splice_children (Container *parent, Container *child);

typedef gboolean (*ContainerForeachFunc) (Container*, gpointer);
gboolean   container_foreach (Container *c,
				     ContainerForeachFunc func,
				     gpointer user_data);

typedef void (*ContainerPathForeachFunc) (Container*, gpointer, Path*);
void   container_path_foreach (Container *c,
					  ContainerPathForeachFunc func,
					  gpointer user_data);

gboolean   container_reachable (Container *haystack, Container *needle);
void       container_dump (Container *c, gboolean recursive);

typedef int (*ContainerCmpFunc) (Container *c1, Container *c2, gpointer user_data);
Container * container_sort (Container *c, GCompareDataFunc func,
			    gpointer user_data, gboolean invert);

#endif /*__MU_THREADER_UTILS_H__*/
