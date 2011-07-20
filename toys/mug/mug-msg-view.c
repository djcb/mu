/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mug-msg-view.h"
#include "mu-msg.h"
#include "mu-str.h"
#include "mu-date.h"

/* 'private'/'protected' functions */
static void mug_msg_view_class_init (MugMsgViewClass * klass);
static void mug_msg_view_init (MugMsgView * obj);
static void mug_msg_view_finalize (GObject * obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

enum _HeaderRow {
	HEADER_ROW_FROM,
	HEADER_ROW_TO,
	HEADER_ROW_SUBJECT,
	HEADER_ROW_CC,
	HEADER_ROW_DATE,
	HEADER_ROW_PATH,
	HEADER_ROW_MSGID,
	HEADER_ROW_SIZE,
	HEADER_ROW_NUM
};
typedef enum _HeaderRow HeaderRow;

struct _HeaderInfo {
	HeaderRow row;
	const char *title;
};
typedef struct _HeaderInfo HeaderInfo;

static const HeaderInfo HEADER_INFO[] = {
	{HEADER_ROW_CC, "Cc"},
	{HEADER_ROW_SUBJECT, "Subject"},
	{HEADER_ROW_DATE, "Date"}
};

static const HeaderInfo HEADER_INFO_EXPANDER[] = {
	{HEADER_ROW_FROM, "From"},
	{HEADER_ROW_TO, "To"},
	{HEADER_ROW_PATH, "Path"},
	{HEADER_ROW_MSGID, "Message-Id"},
	{HEADER_ROW_SIZE, "Size"}
};

typedef struct _MugMsgViewPrivate MugMsgViewPrivate;
struct _MugMsgViewPrivate {

	GtkWidget *_headers_area;
	GtkWidget *_tablemain, *_tableexpander;
	GtkWidget *_headervals[HEADER_ROW_NUM];

	GtkWidget *_expander_header, *_expander;
	GtkWidget *_view;
};
#define MUG_MSG_VIEW_GET_PRIVATE(o)(G_TYPE_INSTANCE_GET_PRIVATE((o),MUG_TYPE_MSG_VIEW, MugMsgViewPrivate))
/* globals */
static GtkVBoxClass *parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

G_DEFINE_TYPE (MugMsgView, mug_msg_view, GTK_TYPE_VBOX);

static void
mug_msg_view_class_init (MugMsgViewClass * klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass *) klass;

	parent_class = g_type_class_peek_parent (klass);
	gobject_class->finalize = mug_msg_view_finalize;

	g_type_class_add_private (gobject_class, sizeof (MugMsgViewPrivate));

	/* signal definitions go here, e.g.: */
/* 	signals[MY_SIGNAL_1] = */
/* 		g_signal_new ("my_signal_1",....); */
/* 	signals[MY_SIGNAL_2] = */
/* 		g_signal_new ("my_signal_2",....); */
/* 	etc. */
}

static GtkWidget *
create_table (MugMsgViewPrivate * priv, const HeaderInfo * hinfo, guint num)
{
	guint i;
	GtkWidget *table;

	table = gtk_table_new (num, 2, FALSE);
	gtk_table_set_col_spacings (GTK_TABLE (table), 5);

	for (i = 0; i < num; ++i) {

		char *str;
		GtkWidget *l, *al;

		l = gtk_label_new (NULL);
		gtk_misc_set_alignment (GTK_MISC (l), 0.0, 0.5);

		gtk_label_set_justify (GTK_LABEL (l), GTK_JUSTIFY_LEFT);
		str = g_strdup_printf ("<b>%s</b>:", hinfo[i].title);
		gtk_label_set_markup (GTK_LABEL (l), str);
		g_free (str);

		al = gtk_alignment_new (0.0, 0.0, 0.0, 0.0);
		gtk_container_add (GTK_CONTAINER (al), l);
		gtk_table_attach (GTK_TABLE (table), al, 0, 1, i, i + 1,
				  GTK_FILL, 0, 0, 0);

		l = priv->_headervals[hinfo[i].row] = gtk_label_new (NULL);
		al = gtk_alignment_new (0.0, 0.0, 0.0, 0.0);
		gtk_label_set_selectable (GTK_LABEL (l), TRUE);

		gtk_container_add (GTK_CONTAINER (al), l);
		gtk_label_set_justify (GTK_LABEL (l), GTK_JUSTIFY_LEFT);
		gtk_label_set_line_wrap_mode (GTK_LABEL (l),
					      PANGO_WRAP_WORD_CHAR);
		gtk_label_set_line_wrap (GTK_LABEL (l), FALSE);
		gtk_table_attach (GTK_TABLE (table), al,
				  1, 2, i, i + 1, GTK_FILL, 0, 0, 0);
	}

	return table;
}

static GtkWidget *
headers_area (MugMsgViewPrivate * priv)
{
	GtkWidget *scrolled, *vbox;

	priv->_tablemain = create_table (priv, HEADER_INFO,
					 G_N_ELEMENTS (HEADER_INFO));
	priv->_tableexpander = create_table
	    (priv, HEADER_INFO_EXPANDER, G_N_ELEMENTS (HEADER_INFO_EXPANDER));
	priv->_expander = gtk_expander_new ("Details");
	gtk_container_add (GTK_CONTAINER (priv->_expander),
			   priv->_tableexpander);

	vbox = gtk_vbox_new (FALSE, FALSE);
	gtk_box_pack_start (GTK_BOX (vbox), priv->_tablemain, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), priv->_expander, FALSE, FALSE, 0);

	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled),
					       vbox);

	return priv->_headers_area = scrolled;
}

static void
mug_msg_view_init (MugMsgView * obj)
{
	MugMsgViewPrivate *priv;
	GtkWidget *scrolled;

	priv = MUG_MSG_VIEW_GET_PRIVATE (obj);

	priv->_view = gtk_text_view_new ();
	gtk_text_view_set_editable (GTK_TEXT_VIEW (priv->_view), FALSE);
	gtk_text_view_set_left_margin (GTK_TEXT_VIEW (priv->_view), 10);
	gtk_text_view_set_right_margin (GTK_TEXT_VIEW (priv->_view), 10);

	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_container_add (GTK_CONTAINER (scrolled), priv->_view);

	gtk_box_pack_start (GTK_BOX (obj), headers_area (priv), FALSE, FALSE,
			    0);
	gtk_box_pack_start (GTK_BOX (obj), scrolled, TRUE, TRUE, 0);
}

static void
mug_msg_view_finalize (GObject * obj)
{
/* 	free/unref instance resources here */
	G_OBJECT_CLASS (parent_class)->finalize (obj);
}

GtkWidget *
mug_msg_view_new (void)
{
	return GTK_WIDGET (g_object_new (MUG_TYPE_MSG_VIEW, NULL));
}

static void
empty_message (MugMsgView * self)
{
	int i;
	MugMsgViewPrivate *priv;
	GtkTextBuffer *buf;

	priv = MUG_MSG_VIEW_GET_PRIVATE (self);

	for (i = 0; i != HEADER_ROW_NUM; ++i)
		gtk_label_set_markup (GTK_LABEL (priv->_headervals[i]), "");

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->_view));
	gtk_text_buffer_set_text (buf, "", -1);
}

static gboolean
set_text (MugMsgView * self, const char *txt)
{
	MugMsgViewPrivate *priv;
	GtkTextBuffer *buf;

	g_return_val_if_fail (MUG_IS_MSG_VIEW (self), FALSE);
	priv = MUG_MSG_VIEW_GET_PRIVATE (self);

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (priv->_view));
	gtk_text_buffer_set_text (buf, txt ? txt : "", -1);

	return TRUE;
}

static void
fill_header (MugMsgViewPrivate * priv, MuMsg * msg)
{
	int i;

	for (i = 0; i != HEADER_ROW_NUM; ++i) {
		const gchar *val;
		switch (i) {
		case HEADER_ROW_FROM: val = mu_msg_get_from (msg); break;
		case HEADER_ROW_TO:   val = mu_msg_get_to (msg); break;
		case HEADER_ROW_SUBJECT: val = mu_msg_get_subject (msg); break;
		case HEADER_ROW_MSGID:	val = mu_msg_get_msgid (msg);break;
		case HEADER_ROW_CC: val = mu_msg_get_cc (msg); break;
		case HEADER_ROW_PATH: val = mu_msg_get_path (msg); break;
		case HEADER_ROW_DATE:
			val = mu_date_str_s ("%c", mu_msg_get_date (msg));
			break;
		case HEADER_ROW_SIZE:
			val = mu_str_size_s (mu_msg_get_size (msg));
			break;
		default:
			val = NULL;
		}

		{
			gchar *str;
			str = g_markup_escape_text (val ? val : "", -1);
			gtk_label_set_markup
			    (GTK_LABEL (priv->_headervals[i]), str);
			g_free (str);
		}
	}
}

gboolean
mug_msg_view_set_msg (MugMsgView * self, const char *msgpath)
{
	MugMsgViewPrivate *priv;
	MuMsg *msg;
	gboolean rv;

	g_return_val_if_fail (MUG_IS_MSG_VIEW (self), FALSE);

	priv = MUG_MSG_VIEW_GET_PRIVATE (self);

	if (!msgpath) {
		empty_message (self);
		return TRUE;
	}

	msg = mu_msg_new_from_file (msgpath, NULL, NULL);
	if (!msg) {
		empty_message (self);
		set_text (self, "Message not found; " "please run 'mu index'");
		return FALSE;
	}

	rv = set_text (self, mu_msg_get_body_text (msg));
	fill_header (priv, msg);

	mu_msg_unref (msg);

	return rv;
}
