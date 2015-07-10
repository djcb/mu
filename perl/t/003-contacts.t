#!/usr/bin/perl
##
# 003-contacts.t - exercise 'contacts' command
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
use Test::More tests => 10;

use t::lib;

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'});
ok($mu,"constructor won");
my $c = $mu->contacts;
ok(ref($c) eq 'HASH',"contacts returned a hashref");
ok(ref($c->{'contacts'}) eq 'ARRAY',"contacts returned an array");
my @clist = map { $_->{'mail'} } @{$c->{'contacts'}};
ok(scalar(@clist) == 5,"right number of contacts in array".join(", ",@clist));
sub have {
    my($a,@l) = @_;
    my $n = grep { $_ eq $a } @l;
    ok($n, "$a is in the list");
}
have('tech@openbsd.org',@clist);
have('habeus@stalphonsos.com',@clist);
have('guenther@gmail.com',@clist);
have('mark.kettenis@xs4all.nl',@clist);
have('slashdot@newsletters.slashdot.org',@clist);
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
