## Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# this is probably too strict
# AC_PREREQ([2.6])

AC_INIT([mu],[0.9.2],[http://code.google.com/p/mu0/issues/list])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/mu.cc])
AM_INIT_AUTOMAKE([dist-bzip2])

# libtoolize wants to put some stuff in here; if you have an old
# autotools/libtool setup. you can try to comment this out
AC_CONFIG_MACRO_DIR([m4])

# silent build; for some older autotools setups, this does not work;
# in that case, simply comment this out
AM_SILENT_RULES([yes])

LT_INIT # don't use AC_PROG_LIBTOOL anymore

AS_IF([test x$prefix = xNONE],[
   prefix=/usr/local])
AC_SUBST(prefix)

AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_CXX
AC_HEADER_STDC

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
AC_CHECK_FUNCS([memset memcpy realpath setlocale strerror])

# require pkg-config
AC_PATH_PROG([PKG_CONFIG], [pkg-config], [no])
AS_IF([test "x$PKG_CONFIG" = "xno"],[
   AC_MSG_ERROR([
   *** The pkg-config script could not be found. Make sure it is
   *** in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.])
])

# glib2?
PKG_CHECK_MODULES(GLIB, glib-2.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)
glib_version="`pkg-config --modversion glib-2.0`"
			  	       
# gtest was introduced in glib 2.16
PKG_CHECK_MODULES(g_test,glib-2.0 >= 2.16, [have_gtest=yes],[have_gtest=no])
AM_CONDITIONAL(HAVE_GTEST, test "x$have_gtest" = "xyes")
AM_CONDITIONAL(HAVE_GLIB216, test "x$have_gtest" = "xyes")
AS_IF([test "x$have_gtest" = "xno"],[
     AC_MSG_WARN([You need GLIB version >= 2.16 to build the tests])
])


# gmime 2.4 or 2.6? mu has only been tested with gmime-2.4, but Fedora
# 14 ships with gmime 2.5.x, which registers itself (pkgconfig) as 2.6
# it is reported mu works fine with this new gmime as well, so we support
# both; based on patch by GooseYArd
PKG_CHECK_MODULES(GMIME,gmime-2.4,[have_gmime_24=yes],[have_gmime_24=no])
AS_IF([test "x$have_gmime_24" = "xno"],[
   PKG_CHECK_MODULES(GMIME,gmime-2.6,[have_gmime_26=yes],[have_gmime_26=no])
   AS_IF([test "x$have_gmime_26" = "xno"],[   
      AC_MSG_ERROR([
      *** neither gmime-2.4 nor gmime-2.6 could not be found; please install it
      *** e.g., in debian/ubuntu the package would be 'libgmime-2.4-dev'
      *** If you compiled it yourself, you should ensure that the pkgconfig
      *** installation dir (e.g., /usr/local/lib/pkgconfig) is in your
      *** PKG_CONFIG_PATH.
      ])],[
         gmime_version="`pkg-config --modversion gmime-2.6`"
    ])],[
      gmime_version="`pkg-config --modversion gmime-2.4`"
   ])
AC_SUBST(GMIME_CFLAGS)
AC_SUBST(GMIME_LIBS)
 


# xapian?
AC_CHECK_PROG(XAPIAN,xapian-config,xapian-config,no)
AM_CONDITIONAL(HAVE_XAPIAN,test "x$XAPIAN" != "xno")
AS_IF([test "x$XAPIAN" = "xno"],[
   AC_MSG_ERROR([
   *** xapian could not be found; please install it
   *** e.g., in debian/ubuntu the package would be 'libxapian-dev'
   *** If you compiled it yourself, you should ensure that xapian-config
   *** is in your PATH.
   ])
],[
   XAPIAN_CXXFLAGS=`$XAPIAN --cxxflags`
   XAPIAN_LIBS=`$XAPIAN --libs`		
   have_xapian="yes"
   xapian_version="`xapian-config --version`"
])   
AC_SUBST(XAPIAN_CXXFLAGS)
AC_SUBST(XAPIAN_LIBS)

# we set the set the version of the Xapian database layout here; it
# will become part of the db name, so we can automatically recreate the
# database when we incompatible have changes.
#
# note that MU_XAPIAN_DB_VERSION does not necessarily follow MU
# versioning, as we hopefully don't have updates for each version;
# also, this has nothing to do with Xapian's software version
AC_DEFINE(MU_XAPIAN_DB_VERSION,["9.0"], [Schema version of the database])


# we test for gtk, which we need if we want to build 'mug'; for
# experiments it can try to build with gtk3, but since that is still
# under development, you need to explicitly enable this with '--with-gtk3'
#
have_gtk=no
AC_ARG_WITH([gtk3],
   [AS_HELP_STRING([--with-gtk3],
   [enable support for gtk3])],
   [],
   [with_gtk3=no])
   
AS_IF([test "x$with_gtk3" != "xno"],[
  PKG_CHECK_MODULES(GTK,gtk+-3.0,[have_gtk3=yes],[have_gtk3=no])
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  gtk_version="`pkg-config --modversion gtk+-3.0`"
])

AS_IF([test "x$have_gtk3" != "xyes"],[
  PKG_CHECK_MODULES(GTK,gtk+-2.0,[have_gtk2=yes],[have_gtk2=no])
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  gtk_version="`pkg-config --modversion gtk+-2.0`"	
])
AM_CONDITIONAL(HAVE_GTK, [test "x$have_gtk2" = "xyes" -o "x$have_gtk3" = "xyes"])

if test "x$have_gtk2" = "xyes" -o "x$have_gtk3" = "xyes"; then
   have_gtk="yes"
else
   have_gtk="no"
fi
   
# check for pmccabe
AC_PATH_PROG([PMCCABE],[pmccabe],[pmccabe],[no])
AS_IF([test "x$PMCCABE" = "xno"],[
   have_pmccabe="no"	    
   AC_MSG_WARN([
   *** Developers: you don't seem to have the 'pmccabe' tool installed.
   *** Please install it if you want to run the automated code checks])
],[
   have_pmccabe="yes"
])

AC_CONFIG_FILES([
Makefile
src/Makefile
src/tests/Makefile
toys/Makefile
toys/mug/Makefile
toys/mug/mug.desktop
man/Makefile
m4/Makefile
contrib/Makefile
])
AC_OUTPUT

echo 
echo "mu configuration is complete."
echo "-----------------------------"

if test -e ~/.mu/xapian-0.6; then
   echo "Note: since version 0.7, the Xapian database is no longer stored as "
   echo "~/.mu/xapian-0.6 but instead simply as ~/.mu/xapian. You can "
   echo "remove the old <muhome>/xapian-0.6 directory to save some disk space"
fi


echo "Xapian version                       : $xapian_version"
echo "GLib version                         : $glib_version" 
echo "GMime version                        : $gmime_version"
if test "x$have_gtk" != "xno"; then
echo "GTK+ version                         : $gtk_version"
fi

echo
echo "Build unit tests (glib >= 2.16)      : $have_gtest"
echo "Build 'mug' (requires GTK+)          : $have_gtk"
echo "McCabe's Cyclomatic Complexity tool  : $have_pmccabe"
echo

echo "Have direntry->d_ino                 : $use_dirent_d_ino" 
echo "Have direntry->d_type                : $use_dirent_d_type" 

echo
echo "type 'make' to build mu, or 'make check' to run the unit tests."
echo "use 'make V=1' to show the detailed output during the build"
