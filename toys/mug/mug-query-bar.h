/* mug-query-bar.h */
/* insert (c)/licensing information) */

#ifndef __MUG_QUERY_BAR_H__
#define __MUG_QUERY_BAR_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG*/

#include <gtk/gtk.h>

G_BEGIN_DECLS
/* convenience macros */
#define MUG_TYPE_QUERY_BAR             (mug_query_bar_get_type())
#define MUG_QUERY_BAR(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MUG_TYPE_QUERY_BAR,MugQueryBar))
#define MUG_QUERY_BAR_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MUG_TYPE_QUERY_BAR,MugQueryBarClass))
#define MUG_IS_QUERY_BAR(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MUG_TYPE_QUERY_BAR))
#define MUG_IS_QUERY_BAR_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MUG_TYPE_QUERY_BAR))
#define MUG_QUERY_BAR_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MUG_TYPE_QUERY_BAR,MugQueryBarClass))
typedef struct _MugQueryBar MugQueryBar;
typedef struct _MugQueryBarClass MugQueryBarClass;

struct _MugQueryBar {
	GtkBox parent;
};

struct _MugQueryBarClass {
	GtkBox parent;
	GtkBoxClass parent_class;
	/* insert signal callback declarations, e.g. */
	void (*query_changed) (MugQueryBar * obj, const char *query);
};

/* member functions */
GType
mug_query_bar_get_type (void)
    G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget *
mug_query_bar_new (void);

void
mug_query_bar_grab_focus (MugQueryBar * self);

void
mug_query_bar_set_query (MugQueryBar * self, const char *query, gboolean run);

G_END_DECLS
#endif				/* __MUG_QUERY_BAR_H__ */
