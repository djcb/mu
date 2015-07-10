#!/usr/bin/perl
##
# 001-basics.t - basic mup tests
##
# Copyright (C) 2015 by attila <attila@stalphonsos.com>
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.
##
use strict;
use warnings;
use Test::More tests => 4;
use Data::Dumper;
use mup;

use t::lib;

# basic tests: constructor, ping, finish

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'});
ok($mu,"constructor won: $mu");
my $p = $mu->ping(timeout => 2);
ok($p,"ping returned:".Dumper($p));
my $p2 = $mu->ping();
ok($p2,"ping2 returned:".Dumper($p2));
ok($mu->finish(),"finish won");

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# perl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# End:
##
