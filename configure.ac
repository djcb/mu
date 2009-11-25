## Copyright (C) 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

# we set the set the version of the metadata database schema here;
# it will become part of the db name, so we can automtically 
# recreate the database when we incompatible changes.
#
# the Xapian and SQLite database share this number, so when
# we upgrade, we can simply update all.
#
# note that MU_DATABASE_VERSION does not necessarily follow
# MU versioning.
#
AC_DEFINE(MU_DATABASE_VERSION,["0.5"], [Schema version of the database])

AC_PROG_LIBTOOL

if test x$prefix = xNONE; then
   prefix=/usr/local
fi
AC_SUBST(prefix)

AC_PROG_CC
AC_PROG_CXX
AM_PROG_CC_STDC
AC_HEADER_STDC

# have pkg-config?
AC_PATH_PROG([PKG_CONFIG], [pkg-config], [no])
if test "x$PKG_CONFIG" = "xno"; then
   AC_MSG_ERROR([
   *** The pkg-config script could not be found. Please make sure it is
   *** in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.])
fi

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
if test "x$XAPIAN" = "xno"; then
      AC_MSG_ERROR([
   *** xapian could not be found; please install it
   *** in debian/ubuntu the package wouldbe 'libxapian-dev'])   
else
   XAPIAN_CXXFLAGS=`$XAPIAN --cxxflags`
   XAPIAN_LIBS=`$XAPIAN --libs`		
   have_xapian="yes"
   AC_DEFINE(HAVE_XAPIAN,[1],[Whether we have Xapian])
fi
AC_SUBST(XAPIAN_CXXFLAGS)
AC_SUBST(XAPIAN_LIBS)


AC_OUTPUT([
Makefile
src/Makefile
])


echo "now, type 'make' to build mu, or 'make test' to run the test suite"
