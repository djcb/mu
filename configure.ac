## Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AC_INIT([mu],[0.9.9-dev7],[http://code.google.com/p/mu0/issues/list],[mu])
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
AC_PROG_INSTALL
AC_HEADER_STDC

# for now, use AM_PROG_LIBTOOL, as we don't want to require
# a too new setup for autotools/libtool
AM_PROG_LIBTOOL
#LT_INIT([disable-shared])

AC_PROG_AWK

AC_CHECK_HEADERS([locale.h langinfo.h wordexp.h])

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
  AS_IF([test "x$lispdir" != "xno"], [emacs_version="`$EMACS --version | head -1`"])
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
PKG_CHECK_MODULES(GLIB,glib-2.0 >= 2.24 gobject-2.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)
glib_version="`$PKG_CONFIG --modversion glib-2.0`"

# gmime 2.4 or 2.6?
PKG_CHECK_MODULES(GMIME,gmime-2.6,[have_gmime_26=yes],[have_gmime_26=no])
AS_IF([test "x$have_gmime_26" = "xno"],[
   PKG_CHECK_MODULES(GMIME,gmime-2.4,[have_gmime_24=yes],[have_gmime_24=no])
   AS_IF([test "x$have_gmime_24" = "xno"],[
      AC_MSG_ERROR([
      *** neither gmime-2.4 nor gmime-2.6 could be found; please install it
      *** e.g., in debian/ubuntu the package would be 'libgmime-2.4-dev'
      *** If you compiled it yourself, you should ensure that the pkgconfig
      *** installation dir (e.g., /usr/local/lib/pkgconfig) is in your
      *** PKG_CONFIG_PATH.
      ])],[
         gmime_version="`$PKG_CONFIG --modversion gmime-2.4`"
    ])],[
      gmime_version="`$PKG_CONFIG --modversion gmime-2.6`"
   ])
AC_SUBST(GMIME_CFLAGS)
AC_SUBST(GMIME_LIBS)

# we need gmime 2.6 for the crypto stuff
AM_CONDITIONAL(BUILD_CRYPTO,[test "x$have_gmime_26" = "xyes"])
AS_IF([test "x$have_gmime_26" = "xyes"],
   AC_DEFINE_UNQUOTED([BUILD_CRYPTO],1,[Whether to build crypto; reqs gmime 2.6]))

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
# recreate the database when we incompatible have changes.
#
# note that MU_STORE_SCHEMA_VERSION does not necessarily follow MU
# versioning, as we hopefully don't have updates for each version;
# also, this has nothing to do with Xapian's software version
AC_DEFINE(MU_STORE_SCHEMA_VERSION,["9.8"], ['Schema' version of the database])
###############################################################################


###############################################################################
# we need gtk (2 or 3) for some of the graphical tools
#
AC_ARG_WITH([gui],
   [AS_HELP_STRING([--with-gui=gtk2|gtk3|none])],
   [gui=$withval],[gui=auto])

AS_IF([test "x$gui" != "xgtk2" -a "x$gui" != "xgtk3" -a "x$gui" != "xnone" \
	-a "x$gui" != "xauto"],
      AC_MSG_ERROR([the argument for --with-gui= must be either \
      			gtk2|gtk3|auto|no ($gui)]))
AS_IF([test "x$gui" != "xnone"],[
   # check for gtk3
   AS_IF([test "x$gui" = "xgtk3"],[
     PKG_CHECK_MODULES(GTK,gtk+-3.0,[have_gtk3=yes],[have_gtk3=no])
     AC_SUBST(GTK_CFLAGS)
     AC_SUBST(GTK_LIBS)
     gtk_version="`$PKG_CONFIG --modversion gtk+-3.0`"
   ])
   AS_IF([test "x$gui"="gtk3" -a "x$have_gtk3" = "xno"],
      AC_MSG_ERROR([GTK+ 3.x not found]))

   # check for gtk2 if we did not find gtk3 already
   # (gtk3 is only sought if asked for it explicitly)
   AS_IF([test "x$gui" != "xno" -a "x$have_gtk3" != "xyes"],[
     PKG_CHECK_MODULES(GTK,gtk+-2.0,[have_gtk2=yes],[have_gtk2=no])
     AC_SUBST(GTK_CFLAGS)
     AC_SUBST(GTK_LIBS)
     gtk_version="`$PKG_CONFIG --modversion gtk+-2.0`"
   ])
   # only an error if we explicitly asked for it
   AS_IF([test "x$have_gtk2" = "xno" -a "x$gui" != "xauto"],
      AC_MSG_ERROR([GTK+ 2.x not found]))
])

AM_CONDITIONAL(HAVE_GTK,[test "x$have_gtk2" = "xyes" -o "x$have_gtk3" = "xyes" ])
AS_IF([test "x$have_gtk2" = "xyes" -o "x$have_gtk3" = "xyes"],[buildgui=yes],
    	    [buildgui=no])
AS_IF([test "x$have_gtk3" = "xyes"],
    [AC_DEFINE_UNQUOTED([HAVE_GTK3],1,[Whether we have GTK+ 3.x])])

# webkit? needed for the fancy web widget
# use --disable-webkit to disable it, even if you have it
# note; gtk2 and gtk3 imply different webkit versions
build_webkit=no
AC_ARG_ENABLE([webkit],
   AS_HELP_STRING([--disable-webkit],[Disable webkit]))
AS_IF([test "x$enable_webkit" != "xno"], [
   AS_IF([test "x$have_gtk2" = "xyes"],[
       PKG_CHECK_MODULES(WEBKIT,webkit-1.0 >= 1.0.3,[build_webkit=yes],[build_webkit=no])
       AS_IF([test "x$build_webkit" = "xyes"],[
	    webkit_version="`$PKG_CONFIG --modversion webkit-1.0`"])
       AC_SUBST(WEBKIT_CFLAGS)
       AC_SUBST(WEBKIT_LIBS)])

   AS_IF([test "x$have_gtk3" = "xyes"],[
       PKG_CHECK_MODULES(WEBKIT,webkitgtk-3.0 >= 1.8.0, [build_webkit=yes],[build_webkit=no])
       AS_IF([test "x$build_webkit" = "xyes"],[
	    webkit_version="`$PKG_CONFIG --modversion webkitgtk-3.0`"])
       AC_SUBST(WEBKIT_CFLAGS)
       AC_SUBST(WEBKIT_LIBS)])
])
AM_CONDITIONAL(BUILD_WEBKIT, [test "x$build_webkit" = "xyes"])


# gio is needed for some widget/ things
AS_IF([test "x$buildgui"="xyes"],[
   PKG_CHECK_MODULES(GIO,gio-2.0,[have_gio=yes],[have_gio=no])
   AS_IF([test "x$have_gio" = "xyes"],[
	    gio_version="`$PKG_CONFIG --modversion gio-2.0`"])
   AC_SUBST(GIO_CFLAGS)
   AC_SUBST(GIO_LIBS)
])
AM_CONDITIONAL(HAVE_GIO, [test "x$have_gio" = "xyes"])

# should we build the gui toys?
AM_CONDITIONAL(BUILD_GUI, [test "x$build_webkit" = "xyes" -a "x$have_gio" = "xyes"])
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
# check for pmccabe
AC_PATH_PROG([PMCCABE],[pmccabe],[pmccabe],[no])
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
lib/tests/Makefile
mu4e/Makefile
mu4e/mu4e-meta.el
mu4e/version.texi
guile/Makefile
guile/mu/Makefile
guile/examples/Makefile
guile/tests/Makefile
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

if test "x$buildgui" = "xyes"; then
echo "GTK+ version                         : $gtk_version"
fi

if test "x$have_gio" = "xyes"; then
echo "GIO version                          : $gio_version"
fi

if test "x$build_webkit" = "xyes"; then
echo "Webkit version                       : $webkit_version"
fi

if test "x$build_guile" = "xyes"; then
echo "Guile version                        : $guile_version"
fi

if test "x$build_mu4e" = "xyes"; then
echo "Emacs version                        : $emacs_version"
fi

echo
echo "Build mu4e emacs frontend            : $build_mu4e"
echo "Build crypto support (gmime >= 2.6)  : $have_gmime_26"
echo "Build 'mug' toy-ui (gtk+/webkit)     : $buildgui"
echo "McCabe's Cyclomatic Complexity tool  : $have_pmccabe"
echo

echo "Have direntry->d_ino                 : $use_dirent_d_ino"
echo "Have direntry->d_type                : $use_dirent_d_type"
echo "------------------------------------------------"
echo

if test "x$have_makeinfo" != "xyes"; then
    echo "You do not seem to have the makeinfo program; if you are building from git"
    echo "you need that to create documentation for guile and emacs. It is in the"
    echo "texinfo package in debian/ubuntu (ie., apt-get install texinfo)"
fi


if test "x$buildgui" = "xyes"; then
     echo "The demo UIs are in toys/mug and toys/mug2"
     if test "x$gui" = "xgtk3"; then
     	echo "Note that mug2 will *not* work with gtk+3, because it depends"
    	echo "on libraries that use gtk+2, and the two can't be in one process"
     fi
fi

if test "x$GUILE_MAJOR_VERSION" = "x1"; then
   echo
   echo "NOTE: If you have troubles with linking the guile-related stuff, it"
   echo "might help to move .la-files out of the way"
   echo
   echo "See e.g: http://blog.flameeyes.eu/2008/04/14/what-about-those-la-files"
fi

echo
echo "Now, type 'make' to build mu."
echo
echo "If unit tests are built (see above), you can run 'make check'"
echo "for some basic testing of mu functionality."
