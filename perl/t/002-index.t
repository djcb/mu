#!/usr/bin/perl
##
# 002-index.t - try the index command
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
use Data::Dumper;
use mup;
use Test::More tests => 4;

use t::lib;

sub update_status {
    my($href) = @_;
    return unless $ENV{'TEST_VERBOSE'};
    local $Data::Dumper::Terse = 1; # cheesy
    local $Data::Dumper::Indent = 0;
    warn("$0: update_status: ".Dumper($href)."\n");
}

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'});
ok($mu,"constructor won");
my $i = $mu->index();
ok($i,"index won: ".Dumper($i));
my $j = $mu->index(callback => \&update_status);
ok($j,"index w/callback won");
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
