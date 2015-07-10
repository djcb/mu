#!/usr/bin/perl
##
# 008-extract.t - test the 'extract' command
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
use Test::More tests => 8;
use Cwd qw(abs_path);
use File::Temp qw/ :POSIX /;

use t::lib;

die("009-extract.t: where is my t/sample.eml?") unless -f "t/sample.eml";
my $sample = abs_path("t/sample.eml");
my $tmpx = tmpnam();
END { unlink($tmpx) if -f $tmpx; }

my $mu = mup->new(verbose => $ENV{'TEST_VERBOSE'});
ok($mu,"constructor won");
my $a = $mu->add(path => $sample);
ok($a,"add seems to have won");
my $id = $a->{'docid'};
ok($id,"new email has docid $id");
ok(!(-f $tmpx),"temp file does not yet exist");
my $x = $mu->extract(docid => $id,path => $tmpx,action => 'save',index => 1);
ok($x,"extract returned something");
ok($x->{'info'} eq 'save',"looks right");
ok(-f $tmpx,"looks like extraction worked");
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
