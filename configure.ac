## Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AC_INIT([mu],[0.6],[http://www.djcbsoftware.nl/code/mu])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/mu.c])
AM_INIT_AUTOMAKE([dist-bzip2])

# we set the set the version of the Xapian database layout here; it
# will become part of the db name, so we can automtically recreate the
# database when we incompatible have changes.
#
# note that MU_XAPIAN_DB_VERSION does not necessarily follow MU
# versioning, as we hopefully don't have updates for each version;
# also, this has nothing to do with the version of the Xapian library
#
AC_DEFINE(MU_XAPIAN_DB_VERSION,["0.6"], [Schema version of the database])

AC_PROG_LIBTOOL

AS_IF([test x$prefix = xNONE],[
   prefix=/usr/local])
AC_SUBST(prefix)

AC_PROG_CC
AC_PROG_CXX
AM_PROG_CC_STDC
AC_HEADER_STDC

# require pkg-config
AC_PATH_PROG([PKG_CONFIG], [pkg-config], [no])
AS_IF([test "x$PKG_CONFIG" = "xno"],[
   AC_MSG_ERROR([
   *** The pkg-config script could not be found. Make sure it is
   *** in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.])
   ])

#
# currently, we don' support systems without d_type in their struct
# dirent (Solaris 10); but we do support FSs for which d_type is always
# DT_UNKNOWN (Like ReiserFS, XFS on Linux)
#
# note, we could work around this if there are many people for which
# this breaks
AC_STRUCT_DIRENT_D_TYPE
AS_IF([test "x$ac_cv_member_struct_dirent_d_type" != "xyes"],[
   AC_MSG_ERROR([
   *** We need the d_type-member in struct dirent, but unfortunately
   *** your system does not seem to have it])
   ])
# support for d_ino in struct dirent is optional     
AC_STRUCT_DIRENT_D_INO


# glib2?
PKG_CHECK_MODULES(GLIB,glib-2.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

# gmime2?	
PKG_CHECK_MODULES(GMIME,gmime-2.4)
AC_SUBST(GMIME_CFLAGS)
AC_SUBST(GMIME_LIBS)

# xapian?
AC_CHECK_PROG(XAPIAN,xapian-config,xapian-config,no)
AM_CONDITIONAL(HAVE_XAPIAN,test "x$XAPIAN" != "xno")
AS_IF([test "x$XAPIAN" = "xno"],[
   AC_MSG_ERROR([
   *** xapian could not be found; please install it
   *** e.g., in debian/ubuntu the package would be 'libxapian-dev'])
],[
   XAPIAN_CXXFLAGS=`$XAPIAN --cxxflags`
   XAPIAN_LIBS=`$XAPIAN --libs`		
   have_xapian="yes"
   AC_DEFINE(HAVE_XAPIAN,[1],[Whether we have Xapian])
])
   
AC_SUBST(XAPIAN_CXXFLAGS)
AC_SUBST(XAPIAN_LIBS)


AC_OUTPUT([
Makefile
src/Makefile
man/Makefile
])


echo
echo "mu configuration is complete."
echo "type 'make' to build it"
