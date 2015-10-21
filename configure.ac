## Copyright (C) 2008-2015 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

AC_INIT([mu],[0.9.15],[https://github.com/djcb/mu/issues],[mu])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([mu/mu.cc])
# libtoolize wants to put some stuff in here; if you have an old
# autotools/libtool setup. you can try to comment this out
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE

# silent build if we have a new enough automake
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AS_IF([test x$prefix = xNONE],[prefix=/usr/local])
AC_SUBST(prefix)

# AC_PROG_CXX *before* AC_PROG_CC, otherwise configure won't error out
# when a c++ compiler is not found. Weird, huh?
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_CC_C99
AC_PROG_INSTALL
AC_HEADER_STDC

# for now, use AM_PROG_LIBTOOL, as we don't want to require
# a too new setup for autotools/libtool
AM_PROG_LIBTOOL
#LT_INIT([disable-shared])

AC_PROG_AWK
AC_CHECK_PROG(SORT,sort,sort)

AC_CHECK_HEADERS([wordexp.h])

# use the 64-bit versions
AC_SYS_LARGEFILE

# check for makeinfo
AC_CHECK_PROG(have_makeinfo,makeinfo,yes,no)
AM_CONDITIONAL(HAVE_MAKEINFO,test "x$have_makeinfo" = "xyes")

# we need emacs for byte-compiling mu4e

build_mu4e=no
AC_ARG_ENABLE([mu4e],
   AS_HELP_STRING([--disable-mu4e],[Disable building mu4e]))
AS_IF([test "x$enable_mu4e" != "xno"], [
  AM_PATH_LISPDIR
  AS_IF([test "x$lispdir" != "xno"], [
    emacs_version="`$EMACS --version | head -1`"
    lispdir="${lispdir}/mu4e/"
  ])
  AS_CASE([$emacs_version],[*23*|*24*|*25*],[build_mu4e=yes],
          [AC_WARN([emacs is too old to build mu4e (need emacs >= 23.x)])])
])
AM_CONDITIONAL(BUILD_MU4E, test "x$build_mu4e" = "xyes")

# we need some special tricks for filesystems that don't have d_type;
# e.g. Solaris. See mu-maildir.c. Explicitly disabling it is for
# testing purposes only
AC_ARG_ENABLE([dirent-d-type],
	AC_HELP_STRING([--disable-dirent-d-type],
			[Don't use dirent->d_type, even if you have it]),
	[], [AC_STRUCT_DIRENT_D_TYPE]
)
AS_IF([test "x$ac_cv_member_struct_dirent_d_type" != "xyes"],
	    [use_dirent_d_type="no"], [use_dirent_d_type="yes"])


# support for d_ino (inode) in struct dirent is optional; if it's
# available we can sort direntries by inode and access them in that
# order; this is much faster on some file systems (such as extfs3).
# Explicity disabling it is for testing purposes only.
AC_ARG_ENABLE([dirent-d-ino],
	AC_HELP_STRING([--disable-dirent-d-ino],
			[Don't use dirent->d_ino, even if you have it]),
	[], [AC_STRUCT_DIRENT_D_INO]
)
AS_IF([test "x$ac_cv_member_struct_dirent_d_ino" != "xyes"],
	    [use_dirent_d_ino="no"], [use_dirent_d_ino="yes"])


# we need these
AC_CHECK_FUNCS([memset memcpy realpath setlocale strerror getpass])
AC_CHECK_FUNC(timegm,[],[AC_MSG_ERROR([missing required function timegm])])

# require pkg-config
AC_PATH_PROG([PKG_CONFIG], [pkg-config], [no])
AS_IF([test "x$PKG_CONFIG" = "xno"],[
   AC_MSG_ERROR([
   *** The pkg-config script could not be found. Make sure it is
   *** in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.])
])

# glib2?
# we need 2.14 at least, because we use GRegex
PKG_CHECK_MODULES(GLIB,glib-2.0 >= 2.24 gobject-2.0 gio-2.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)
glib_version="`$PKG_CONFIG --modversion glib-2.0`"

# gmime
PKG_CHECK_MODULES(GMIME,gmime-2.6 >= 2.6.7)
AC_SUBST(GMIME_CFLAGS)
AC_SUBST(GMIME_LIBS)
gmime_version="`$PKG_CONFIG --modversion gmime-2.6`"


# xapian checking - we need 1.2.x at least
AC_CHECK_PROG(XAPIAN_CONFIG,xapian-config,xapian-config,no)
AM_CONDITIONAL(HAVE_XAPIAN,test "x$XAPIAN_CONFIG" != "xno")
AS_IF([test "x$XAPIAN_CONFIG" = "xno"],[
   AC_MSG_ERROR([
   *** xapian could not be found; please install it
   *** e.g., in debian/ubuntu the package would be 'libxapian-dev'
   *** If you compiled it yourself, you should ensure that xapian-config
   *** is in your PATH.])],
   [xapian_version=$($XAPIAN_CONFIG --version | sed -e 's/.* //')])
AS_CASE([$xapian_version],
	[1.[[2-9]].[[0-9]]*], [],
	[AC_MSG_ERROR([*** xapian version >= 1.2 needed,
	but version $xapian_version found.])])

XAPIAN_CXXFLAGS="`$XAPIAN_CONFIG --cxxflags`"
XAPIAN_LIBS="`$XAPIAN_CONFIG --libs`"
have_xapian="yes"

AC_SUBST(XAPIAN_CXXFLAGS)
AC_SUBST(XAPIAN_LIBS)

###############################################################################
# we set the set the version of the MuStore (Xapian database) layout
# here; it will become part of the db name, so we can automatically
# recreate the database when we have incompatible changes.
#
# note that MU_STORE_SCHEMA_VERSION does not necessarily follow MU
# versioning, as we hopefully don't have updates for each version;
# also, this has nothing to do with Xapian's software version
AC_DEFINE(MU_STORE_SCHEMA_VERSION,["9.9"], ['Schema' version of the database])
###############################################################################


###############################################################################
# we need GTK+3 for some of the graphical tools
# use --without-gtk to disable it
AC_ARG_ENABLE([gtk],AS_HELP_STRING([--disable-gtk],[Disable GTK+]))
AS_IF([test "x$enable_gtk" != "xno"],[
     PKG_CHECK_MODULES(GTK,gtk+-3.0,[have_gtk=yes],[have_gtk=no])
     AC_SUBST(GTK_CFLAGS)
     AC_SUBST(GTK_LIBS)
     gtk_version="`$PKG_CONFIG --modversion gtk+-3.0`"])
AM_CONDITIONAL(HAVE_GTK,[test "x$have_gtk" = "xyes"])

# webkit? needed for the fancy web widget
# use --disable-webkit to disable it, even if you have it
AC_ARG_ENABLE([webkit],AS_HELP_STRING([--disable-webkit],[Disable webkit]))
AS_IF([test "x$enable_webkit" != "xno"],[
       PKG_CHECK_MODULES(WEBKIT,webkitgtk-3.0 >= 1.8.0,[have_webkit=yes],[have_webkit=no])
       AS_IF([test "x$have_webkit" = "xyes"],[
	    webkit_version="`$PKG_CONFIG --modversion webkitgtk-3.0`"])
       AC_SUBST(WEBKIT_CFLAGS)
       AC_SUBST(WEBKIT_LIBS)
])
AM_CONDITIONAL(HAVE_WEBKIT, [test "x$have_webkit" = "xyes"])
AM_CONDITIONAL(BUILD_GUI,[test "x$have_webkit" = "xyes" -a "x$have_gtk" = "xyes"])
###############################################################################



###############################################################################
# check for guile 2.0 & guile-snarf
build_guile=no
AC_ARG_ENABLE([guile],
   AS_HELP_STRING([--disable-guile],[Disable guile]))

AS_IF([test "x$enable_guile" != "xno"], [
   PKG_CHECK_MODULES(GUILE,guile-2.0,[build_guile=yes],[build_guile=no])
   AC_SUBST(GUILE_CFLAGS)
   AC_SUBST(GUILE_LIBS)

   GUILE_EXECDIR="`${PKG_CONFIG} guile-2.0 --variable=exec_prefix`/bin"

   AS_IF([test "x$build_guile" = "xyes"],[
   AC_PATH_PROG(GUILE_BINARY, [guile], [], [$GUILE_EXECDIR])
      AS_IF([test "x$GUILE_BINARY" != "x"],
         [AC_DEFINE_UNQUOTED([GUILE_BINARY],["$GUILE_BINARY"],[Path to the guile binary])],
         [AC_MSG_WARN([cannot find guile-snarf])])])

   AS_IF([test "x$build_guile" = "xyes"],[
      AC_PATH_PROG(GUILE_SNARF, [guile-snarf], [], [$GUILE_EXECDIR])
      AS_IF([test "x$GUILE_SNARF" != "x"],
         [AC_DEFINE_UNQUOTED([GUILE_SNARF],["$GUILE_SNARF"],[Path to guile-snarf])
         GUILE_SITEDIR="`${PKG_CONFIG} guile-2.0 --variable=sitedir`"],
         [AC_MSG_WARN([cannot find guile-snarf])])])
   AC_SUBST(GUILE_SITEDIR)])

AS_IF([test "x$build_guile" = "xyes" -a "x$GUILE_SNARF" != "x"],
      [guile_version="`${PKG_CONFIG} --modversion guile-2.0`"
      AC_DEFINE_UNQUOTED([BUILD_GUILE],1,[Whether we build with guile 2.0 support])])
AM_CONDITIONAL(BUILD_GUILE, [test "x$build_guile" = "xyes"])
###############################################################################



###############################################################################
# check for makeinfo
AC_CHECK_PROG(have_makeinfo,makeinfo,yes,no)
AM_CONDITIONAL(HAVE_MAKEINFO, [test "x$have_makeinfo" = "xyes"])
###############################################################################



###############################################################################
# docdir, so we can use it in mu4e-meta.el.in
AC_SUBST(MU_DOC_DIR, "${prefix}/share/doc/mu")
###############################################################################



###############################################################################
# check for pmccabe
AC_PATH_PROG([PMCCABE],[pmccabe],[no])
AS_IF([test "x$PMCCABE" = "xno"],[
   have_pmccabe="no"
   AC_MSG_WARN([
   *** Developers: you do not seem to have the pmccabe tool installed.
   *** Please install it if you want to run the automated code checks])
],[have_pmccabe="yes"])
###############################################################################

AC_CONFIG_FILES([
Makefile
mu/Makefile
mu/tests/Makefile
lib/Makefile
lib/doxyfile
lib/tests/Makefile
mu4e/Makefile
mu4e/mu4e-meta.el
mu4e/texi.texi
guile/Makefile
guile/texi.texi
guile/mu/Makefile
guile/examples/Makefile
guile/tests/Makefile
guile/scripts/Makefile
toys/Makefile
toys/mug/Makefile
toys/msg2pdf/Makefile
man/Makefile
m4/Makefile
contrib/Makefile
])
AC_OUTPUT

echo
echo "mu configuration is complete."
echo "------------------------------------------------"

if test -e ~/.mu/xapian-0.6; then
   echo "Note: since version 0.7, the Xapian database is no longer stored as "
   echo "~/.mu/xapian-0.6 but instead simply as ~/.mu/xapian. You can "
   echo "remove the old <muhome>/xapian-0.6 directory to save some disk space"
fi

echo "mu version                           : $VERSION"
echo
echo "Xapian version                       : $xapian_version"
echo "GLib version                         : $glib_version"
echo "GMime version                        : $gmime_version"

AM_COND_IF([BUILD_GUI],[
echo "GTK+ version                         : $gtk_version"
echo "Webkit version                       : $webkit_version"
])

AM_COND_IF([BUILD_GUILE],[
echo "Guile version                        : $guile_version"
])


if test "x$build_mu4e" = "xyes"; then
echo "Emacs version                        : $emacs_version"
fi

echo
echo "Have wordexp                         : $ac_cv_header_wordexp_h"
echo "Build mu4e emacs frontend            : $build_mu4e"

AM_COND_IF([BUILD_GUI],[
echo "Build 'mug' toy-ui (gtk+/webkit)     : yes"],[
echo "Build 'mug' toy-ui (gtk+/webkit)     : no"
])


echo "McCabe's Cyclomatic Complexity tool  : $have_pmccabe"
echo

echo "Have direntry->d_ino                 : $use_dirent_d_ino"
echo "Have direntry->d_type                : $use_dirent_d_type"
echo "------------------------------------------------"
echo

#
# Warnings / notes
#

# makeinfo
if test "x$have_makeinfo" != "xyes"; then
    echo "* You do not seem to have the makeinfo program; if you are building from git"
    echo "  you need that to create documentation for guile and emacs. It is in the"
    echo "  texinfo package in debian/ubuntu/fedora/... "
    echo
fi

# gui
if test "x$buildgui" = "xyes"; then
     echo "* The demo UI will be built in toys/mug"
     echo
fi

# wordexp
if test "x$ac_cv_header_wordexp_h" != "xyes"; then
   echo "* Your system does not seem to have the 'wordexp' function."
   echo "  This means that you cannot use shell-like expansion in options and "
   echo "  some other places. So, for example, instead of"
   echo "    --maildir=~/Maildir"
   echo "  you should use the complete path, something like:"
   echo "    --maildir=/home/user/Maildir"
fi

echo
echo "Now, type 'make' to build mu (or 'make check' to run the unit tests, too)"
echo
