/*
** Copyright (C) 2014  Jakub Sitnicki <jsitnicki@gmail.com>
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

#include "config.h"
#include <glib.h>

#include "utils/mu-test-utils.hh"
#include "mu-container.hh"

static gboolean
container_has_children(const MuContainer* c)
{
	return c && c->child;
}

static gboolean
container_is_sibling_of(const MuContainer* c, const MuContainer* sibling)
{
	const MuContainer* cur;

	for (cur = c; cur; cur = cur->next) {
		if (cur == sibling)
			return TRUE;
	}

	return container_is_sibling_of(sibling, c);
}

static void
test_mu_container_splice_children_when_parent_has_no_siblings(void)
{
	MuContainer *child, *parent, *root_set;

	child  = mu_container_new(NULL, 0, "child");
	parent = mu_container_new(NULL, 0, "parent");
	parent = mu_container_append_children(parent, child);

	root_set = parent;
	root_set = mu_container_splice_children(root_set, parent);

	g_assert(root_set != NULL);
	g_assert(!container_has_children(parent));
	g_assert(container_is_sibling_of(root_set, child));

	mu_container_destroy(parent);
	mu_container_destroy(child);
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/mu-container/mu-container-splice-children-when-parent-has-no-siblings",
	                test_mu_container_splice_children_when_parent_has_no_siblings);

	g_log_set_handler(
	    NULL,
	    (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
	    (GLogFunc)black_hole,
	    NULL);

	return g_test_run();
}
