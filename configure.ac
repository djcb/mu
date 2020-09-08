## Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

AC_PREREQ([2.68])
AC_INIT([mu],[1.4.13],[https://github.com/djcb/mu/issues],[mu])
AC_COPYRIGHT([Copyright (C) 2008-2020 Dirk-Jan C. Binnema])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([mu/mu.cc])
# libtoolize wants to put some stuff in here; if you have an old
# autotools/libtool setup. you can try to comment this out
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

m4_ifdef([AX_IS_RELEASE],[AX_IS_RELEASE([git-directory])])
m4_ifdef([AX_CHECK_ENABLE_DEBUG],[AX_CHECK_ENABLE_DEBUG([yes])])

AM_INIT_AUTOMAKE([1.14 foreign no-dist-gzip tar-ustar dist-xz])

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

extra_flags="-Wformat-security                                        \
             -Wstack-protector                                        \
             -Wstack-protector-all                                    \
             -Wno-cast-function-type"

AX_CXX_COMPILE_STDCXX_14
m4_ifdef([AX_COMPILER_FLAGS],[AX_COMPILER_FLAGS(,,[yes],${extra_flags})])
AX_VALGRIND_CHECK

LT_INIT

AX_CODE_COVERAGE

AC_PROG_AWK
AC_CHECK_PROG(SORT,sort,sort)

AC_CHECK_HEADERS([wordexp.h])

# use the 64-bit versions
AC_SYS_LARGEFILE

# asan is somewhat similar to valgrind, but has low enough overhead so it
# can be used during normal operation.
AC_ARG_ENABLE([asan],[AS_HELP_STRING([--enable-asan],
              [Enable Address Sanitizer])], [use_asan=$enableval], [use_asan=no])
AS_IF([test "x$use_asan" = "xyes"],[
  AC_SUBST(ASAN_CFLAGS,  "-fsanitize=address -static-libasan -fno-omit-frame-pointer")
  AC_SUBST(ASAN_CXXFLAGS,"-fsanitize=address -static-libasan -fno-omit-frame-pointer")
  AC_SUBST(ASAN_LDFLAGS, "-fsanitize=address -static-libasan -fno-omit-frame-pointer")
])

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
    emacs_version="$($EMACS --version | head -1)"
    lispdir="${lispdir}/mu4e/"
  ])
  AS_CASE([$emacs_version],
    [*24.4*|*24.5*],[build_mu4e=yes],
    [*25*|*26*|*27*|*28*],[build_mu4e=yes],
    [AC_WARN([emacs is too old to build mu4e (need emacs >= 24.4)])])
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
# Explicitly disabling it is for testing purposes only.
AC_ARG_ENABLE([dirent-d-ino],
    AC_HELP_STRING([--disable-dirent-d-ino],
            [Don't use dirent->d_ino, even if you have it]),
    [], [AC_STRUCT_DIRENT_D_INO]
)
AS_IF([test "x$ac_cv_member_struct_dirent_d_ino" != "xyes"],
        [use_dirent_d_ino="no"], [use_dirent_d_ino="yes"])

AC_CHECK_FUNCS([memset memcpy realpath setlocale strerror getpass setsid])
AC_CHECK_FUNCS([vasprintf strptime])
AC_CHECK_FUNC(timegm,[],AC_MSG_ERROR([missing required function timegm]))

# require pkg-config >= 0.28 (release in 2013; should be old enough...)
# with that version, we don't need the AC_SUBST stuff after PKG_CHECK.
m4_ifndef([PKG_PROG_PKG_CONFIG],
   [m4_fatal([please install pkg-config >= 0.28 before running autoconf/autogen])])
PKG_PROG_PKG_CONFIG(0.28) # latest version in buildroot
AS_IF([test -z "$PKG_CONFIG"],
   AC_MSG_ERROR([
   *** pkg-config with version >= 0.28 could not be found.
   ***
   *** Make sure it is in your path, or set the PKG_CONFIG environment variable
   *** to the full path to pkg-config.])
)

# glib2?
PKG_CHECK_MODULES(GLIB,glib-2.0 >= 2.38 gobject-2.0 gio-2.0)
glib_version="$($PKG_CONFIG --modversion glib-2.0)"

# gmime, version 3.0 or higher
PKG_CHECK_MODULES(GMIME,gmime-3.0)
gmime_version="$($PKG_CONFIG --modversion gmime-3.0)"

# gmime, version 3.0 or higher
PKG_CHECK_MODULES(JSON_GLIB,json-glib-1.0 >= 1.4,[have_json_glib=yes],[have_json_glib=no])
AS_IF([test "x$have_json_glib" = "xyes"],[
  json_glib_version="$($PKG_CONFIG --modversion json-glib-1.0)"
  AC_DEFINE(HAVE_JSON_GLIB,[1], [Do we support json-glib?])
])
AM_CONDITIONAL(HAVE_JSON_GLIB,[test "x$have_json_glib" = "xyes"])

# xapian checking - we need 1.4 at least
PKG_CHECK_MODULES(XAPIAN,xapian-core >= 1.4,[
  have_xapian=yes
  xapian_version=$($PKG_CONFIG xapian-core --modversion)
  AC_SUBST(XAPIAN_CXXFLAGS,${XAPIAN_CFLAGS})
],[
  # fall back to the xapian-config script. Not sure if there are cases where the
  # pkgconfig does not work, but xapian-config does, so keep this for now.
  AC_MSG_NOTICE([falling back to xapian-config])
  AC_CHECK_PROG(XAPIAN_CONFIG,xapian-config,xapian-config,no)
  AS_IF([test "x$XAPIAN_CONFIG" = "xno"],[
     AC_MSG_ERROR([
     *** xapian could not be found; please install it
     *** e.g., in debian/ubuntu the package would be 'libxapian-dev'
     *** If you compiled it yourself, you should ensure that xapian-config
     *** is in your PATH.])],
     [xapian_version=$($XAPIAN_CONFIG --version | sed -e 's/.* //')])

  AS_CASE([$xapian_version],
      [1.[[4-9]].[[0-9]]*],
        [AC_MSG_NOTICE([xapian $xapian_version found.])],
        [AC_MSG_ERROR([*** xapian version >= 1.4 needed, but version $xapian_version found.])])

  XAPIAN_CXXFLAGS="$($XAPIAN_CONFIG --cxxflags)"
  XAPIAN_LIBS="$($XAPIAN_CONFIG --libs)"
  have_xapian="yes"

  AC_SUBST(XAPIAN_CXXFLAGS)
  AC_SUBST(XAPIAN_LIBS)
])
###############################################################################
# we set the set the version of the MuStore (Xapian database) layout
# here; it will become part of the db name, so we can automatically
# recreate the database when we have incompatible changes.
#
# note that MU_STORE_SCHEMA_VERSION does not follow mu versioning, as we
# hopefully don't have updates for each version; also, this has nothing to do
# with Xapian's software version
AC_DEFINE(MU_STORE_SCHEMA_VERSION,["451"],['Schema' version of the database])
###############################################################################

###############################################################################
# we need GTK+3 for some of the graphical tools
# use --without-gtk to disable it
AC_ARG_ENABLE([gtk],AS_HELP_STRING([--disable-gtk],[Disable GTK+]))
AS_IF([test "x$enable_gtk" != "xno"],[
     PKG_CHECK_MODULES(GTK,gtk+-3.0,[have_gtk=yes],[have_gtk=no])
     gtk_version="$($PKG_CONFIG --modversion gtk+-3.0)"
])
AM_CONDITIONAL(HAVE_GTK,[test "x$have_gtk" = "xyes"])

# webkit? needed for the fancy web widget
# use --disable-webkit to disable it, even if you have it
#
# and note this is just a toy, not for distribution.
AC_ARG_ENABLE([webkit],AS_HELP_STRING([--disable-webkit],[Disable webkit]))
AS_IF([test "x$enable_webkit" != "xno"],[
       PKG_CHECK_MODULES(WEBKIT,webkit2gtk-4.0 >= 2.0, [have_webkit=yes],[have_webkit=no])
       AS_IF([test "x$have_webkit" = "xyes"],[
        webkit_version="$($PKG_CONFIG --modversion webkit2gtk-4.0)"])
])
AM_CONDITIONAL(HAVE_WEBKIT, [test "x$have_webkit" = "xyes"])
AM_CONDITIONAL(BUILD_GUI,[test "x$have_webkit" = "xyes" -a "x$have_gtk" = "xyes"])
###############################################################################

###############################################################################
# build with guile 3.0/2.2 when available and not disabled.
AC_ARG_ENABLE([guile], AS_HELP_STRING([--disable-guile],[Disable guile]))
AS_IF([test "x$enable_guile" != "xno"],[
  PKG_CHECK_MODULES(GUILE, [guile-2.2], [have_guile=yes], [have_guile=no])
  PKG_CHECK_MODULES(GUILE, [guile-3.0], [have_guile=yes])
  AS_IF([test "x$have_guile" = "xyes"],[
    GUILE_PKG([3.0 2.2])
    GUILE_PROGS
    GUILE_FLAGS
    AC_DEFINE_UNQUOTED([GUILE_BINARY],"$GUILE",[guile binary])
    AC_DEFINE(BUILD_GUILE,[1], [Do we support Guile?])
    AC_SUBST(GUILE_SNARF, [guile-snarf])
    guile_version=$($PKG_CONFIG guile-$GUILE_EFFECTIVE_VERSION --modversion)
  ])
])
AM_CONDITIONAL(BUILD_GUILE,[test "x$have_guile" = "xyes"])
###############################################################################

###############################################################################
# optional readline
saved_libs=$LIBS
AX_LIB_READLINE
AC_SUBST(READLINE_LIBS,${LIBS})
LIBS=$saved_libs
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

AC_CONFIG_FILES([
Makefile
mu/Makefile
mu/mu-memcheck
lib/Makefile
lib/doxyfile
lib/utils/Makefile
lib/query/Makefile
mu4e/Makefile
mu4e/mu4e-meta.el
guile/Makefile
guile/texi.texi
guile/mu/Makefile
guile/examples/Makefile
guile/tests/Makefile
guile/scripts/Makefile
toys/Makefile
toys/mug/Makefile
man/Makefile
m4/Makefile
contrib/Makefile
],[
 [chmod +x mu/mu-memcheck]
])
AC_OUTPUT

dnl toys/msg2pdf/Makefile

echo
echo "mu configuration is complete."
echo "------------------------------------------------"

echo "mu version                           : $VERSION"
echo
echo "Xapian version                       : $xapian_version"
echo "GLib version                         : $glib_version"
echo "GMime version                        : $gmime_version"

AM_COND_IF([HAVE_JSON_GLIB],[
echo "Json-Glib version                    : $json_glib_version"
])

AM_COND_IF([BUILD_GUI],[
echo "GTK+ version                         : $gtk_version"
echo "Webkit2/GTK+ version                 : $webkit_version"
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
AS_IF([test "x$buildgui" = "xyes"],[
     echo "* The demo UI will be built in toys/mug"
     echo
])

# wordexp
AS_IF([test "x$ac_cv_header_wordexp_h" != "xyes"],[
   echo "* Your system does not seem to have the 'wordexp' function."
   echo "  This means that you cannot use shell-like expansion in options and "
   echo "  some other places. So, for example, instead of"
   echo "    --maildir=~/Maildir"
   echo "  you should use the complete path, something like:"
   echo "    --maildir=/home/user/Maildir"
])

echo
echo "Now, type 'make' (or 'gmake') to build mu"
echo
