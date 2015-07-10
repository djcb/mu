# perlmod.m4 - Check for the availability of a perl module -*-Autoconf-*-
#
# Copyright (C) 2015 by attila <attila@stalphonsos.com>
#
# Permission to use, copy, modify, and distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

# AM_PERL_MODULE([module_name],[code_to_execute])
# -----------------------------------------------

# serial 1

AC_DEFUN([AM_PERL_MODULE],
[AC_PREREQ([2.60])dnl
 AC_MSG_CHECKING([for $1])
 _modfile=`$PERL -e 'use '$1'; my $p="'$1'"; $p =~ s/::/\//g; print $INC{"$p.pm"}."\n";' 2>/dev/null`
 if test -z "$_modfile"; then
    AC_MSG_WARN([missing $1])
 else
    AC_MSG_RESULT([$_modfile])
    m4_default([$2],[:])
 fi
])dnl
