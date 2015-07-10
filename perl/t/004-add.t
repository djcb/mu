#!/usr/bin/perl
##
# 004-add.t - try the 'add' command
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
use mup;
use Test::More tests => 3;
use Cwd qw(abs_path);
use Data::Dumper;

use t::lib;

sub up {
    return unless $ENV{'TEST_VERBOSE'};
    my($upd) = @_;
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    warn("004-add.t: update: ".Dumper($upd)."\n");
}

die("004-add.t: where is my t/sample.eml?") unless -f "t/sample.eml";
my $sample = abs_path("t/sample.eml");

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'}, update_callback => \&up);
ok($mu,"constructor won");
my $a = $mu->add(path => $sample);
ok($a,"add won: ".Dumper($a));
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
