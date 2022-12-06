## Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
AC_INIT([mu],[1.8.12],[https://github.com/djcb/mu/issues],[mu])
AC_COPYRIGHT([Copyright (C) 2008-2022 Dirk-Jan C. Binnema])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([mu/mu.cc])
# libtoolize wants to put some stuff in here; if you have an old
# autotools/libtool setup. you can try to comment this out
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.14 foreign no-dist-gzip tar-ustar dist-xz])

# silent build if we have a new enough automake
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AS_IF([test x$prefix = xNONE],[prefix=/usr/local])
AC_SUBST(prefix)

# AC_PROG_CXX *before* AC_PROG_CC, otherwise configure won't error out
# when a c++ compiler is not found. Weird, huh?
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP


extra_flags="-Wformat-security		\
         -Wstack-protector		\
         -Wstack-protector-all		\
         -Wno-cast-function-type	\
         -Wno-bad-function-cast		\
	 -Wno-switch-enum"

AX_CXX_COMPILE_STDCXX_17
AX_COMPILER_FLAGS_CXXFLAGS([],[],[${extra_cflags}])
AX_APPEND_COMPILE_FLAGS([-Wno-inline],[CXXFLAGS])
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
AM_COND_IF(HAVE_MAKEINFO,[],[
  # seems build *insists* on trying to makeinfo, erroring out
  # if it does not exist. Let's work around that.
  AC_SUBST(MAKEINFO,[true])
])

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
    [*25.3*],[build_mu4e=yes],
    [*26*|*27*|*28*|*29*],[build_mu4e=yes],
    [AC_MSG_WARN(emacs is too old to build mu4e (need emacs >= 25.3))])
])
AM_CONDITIONAL(BUILD_MU4E, test "x$build_mu4e" = "xyes")

# we need some special tricks for filesystems that don't have d_type;
# e.g. Solaris. See mu-maildir.c. Explicitly disabling it is for
# testing purposes only
AC_ARG_ENABLE([dirent-d-type],
    AS_HELP_STRING([--disable-dirent-d-type],[Don't use dirent->d_type, even if you have it]),
    [], [AC_STRUCT_DIRENT_D_TYPE]
)
AS_IF([test "x$ac_cv_member_struct_dirent_d_type" != "xyes"],
    [use_dirent_d_type="no"], [use_dirent_d_type="yes"])

# support for d_ino (inode) in struct dirent is optional; if it's
# available we can sort direntries by inode and access them in that
# order; this is much faster on some file systems (such as extfs3).
# Explicitly disabling it is for testing purposes only.
AC_ARG_ENABLE([dirent-d-ino],
    AS_HELP_STRING([--disable-dirent-d-ino],[Don't use dirent->d_ino, even if you have it]),
    [], [AC_STRUCT_DIRENT_D_INO]
)
AS_IF([test "x$ac_cv_member_struct_dirent_d_ino" != "xyes"],
    [use_dirent_d_ino="no"], [use_dirent_d_ino="yes"])

AC_CHECK_FUNCS([memset memcpy realpath setlocale strerror getpass setsid])
AC_CHECK_FUNCS([vasprintf strptime])
# timegm is no longer used in the source
# AC_CHECK_FUNC(timegm,[],AC_MSG_ERROR([missing required function timegm]))

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
PKG_CHECK_MODULES(GLIB,glib-2.0 >= 2.58 gobject-2.0 gio-2.0)
glib_version="$($PKG_CONFIG --modversion glib-2.0)"

# gmime, version 3.0 or higher
PKG_CHECK_MODULES(GMIME,gmime-3.0)
gmime_version="$($PKG_CONFIG --modversion gmime-3.0)"

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
# with Xapian's software versionx
AC_DEFINE(MU_STORE_SCHEMA_VERSION,["465"],['Schema' version of the database])
###############################################################################

################################################################################
# should we try to build an emacs dynamic module?
#AC_CHECK_HEADER([emacs-module.h],[
#  AC_DEFINE([HAVE_EMACS_MODULE_H],[1], [Whether we have the emacs-module header])],
#  AC_MSG_NOTICE([emacs-module.h not found; not building module])
#)
#AM_CONDITIONAL([BUILD_EMACS_MODULE],[test "x$ac_cv_header_emacs_module_h" != "x"])
################################################################################

###############################################################################
# build with guile 3.0/2.2 when available and not disabled.
AC_ARG_ENABLE([guile], AS_HELP_STRING([--disable-guile],[Disable guile]))
AS_IF([test "x$enable_guile" != "xno"],[

  PKG_CHECK_MODULES(GUILE, [guile-3.0], [have_guile=yes],[
    PKG_CHECK_MODULES(GUILE, [guile-2.2], [have_guile=yes], [have_guile=no])])
    AS_IF([test "x$have_guile" = "xyes"],[
      GUILE_PKG([3.0 2.2])
      GUILE_PROGS
      GUILE_FLAGS
      AC_DEFINE_UNQUOTED([GUILE_BINARY],"$GUILE",[guile binary])
      vsnarf=guile-snarf${GUILE_EFFECTIVE_VERSION}
      AC_CHECK_PROGS(GUILE_SNARF,[${vsnarf} guile-snarf], [no])
      guile_version=$($PKG_CONFIG guile-$GUILE_EFFECTIVE_VERSION --modversion)
   ])
])

AM_CONDITIONAL(BUILD_GUILE,[test "x$have_guile" = "xyes" -a \
                 "x$ac_cv_prog_GUILE_SNARF" != "xno"])
AM_COND_IF([BUILD_GUILE],[AC_DEFINE(BUILD_GUILE,[1], [Do we support Guile?])])
###############################################################################

###############################################################################
# optional readline
AC_ARG_ENABLE([readline], AS_HELP_STRING([--disable-readline],[Disable readline]))
AS_IF([test "x$enable_readline" != "xno"], [
  saved_libs=$LIBS
  AX_LIB_READLINE
  AC_SUBST(READLINE_LIBS,${LIBS})
  LIBS=$saved_libs
])
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
lib/Makefile
lib/doxyfile
lib/thirdparty/Makefile
lib/utils/Makefile
lib/message/Makefile
lib/index/Makefile
mu4e/Makefile
mu4e/mu4e-config.el
guile/Makefile
guile/mu/Makefile
guile/examples/Makefile
guile/scripts/Makefile
man/Makefile
m4/Makefile
contrib/Makefile
])
AC_CONFIG_FILES([mu/mu-memcheck], [chmod +x mu/mu-memcheck])

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

AM_COND_IF([BUILD_GUILE],[
echo "Guile version                        : $guile_version"
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

# wordexp
AS_IF([test "x$ac_cv_header_wordexp_h" != "xyes"],[
   echo "* Your system does not seem to have the 'wordexp' function."
   echo "  This means that you cannot use shell-like expansion in options and "
   echo "  some other places. So, for example, instead of"
   echo "    --maildir=~/Maildir"
   echo "  you should use the complete path, something like:"
   echo "    --maildir=/home/user/Maildir"
])


echo "NOTE: autotools support has been deprecated and will be removed"
echo "      after the next stable release. use the meson build instead"

echo
echo "Now, type 'make' (or 'gmake') to build mu"
echo
