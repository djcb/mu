#! perl
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
 
package main;
use File::Path qw(make_path remove_tree);

# ensure a testworthy Maildir is available and pointed at by $ENV{MAILDIR}

sub setup_testing_envars {
    if (-x '../../mu/mu') {
        $ENV{'MUP_MU_BIN'} = '../../mu/mu';
    }
    if (-x '../mu/mu') {
        $ENV{'MUP_MU_BIN'} = '../mu/mu';
    }
    warn("MUP_MU_BIN is $ENV{MUP_MU_BIN}\n") if $ENV{'TEST_VERBOSE'};
}

sub setup_testing_maildir {
    my $verbose = $ENV{'TEST_VERBOSE'};
    if ($ENV{'MAILDIR'}) {
        warn("setup_testing_maildir: MAILDIR is already ".$ENV{'MAILDIR'}."\n")
            if $verbose;
        return;
    }
    my $tmpd = $ENV{'TMPDIR'} || '/tmp';
    my $md = $tmpd . "/mup_t_maildir.$$";
    my $cmd;
    # 1. ensure the test maildir did not exist first and then create it
    die("setup_testing_maildir: $md exists!") if (-d $md || -f $md);
    make_path($md) or die("setup_testing_maildir: mkdir $md: $!");
    $ENV{'MAILDIR'} = $md;
    $ENV{'MUP_TEST_MAILDIR_SET'} = 1;
    warn("setup_testing_maildir: $md\n") if $verbose;
    # 2. populate it with some test data
    if (!(-d "t/sample.maildir")) {
        warn("setup_testing_maildir: no t/sample.maildir available!\n")
            if $verbose;
    } else {
        my $tar = $ENV{'MUP_TEST_TAR'} || 'tar';
        my $xf = $verbose ? 'xvf' : 'xf';
        $cmd = qq{sh -c '(cd t/sample.maildir; ${tar} -cf - .) | (cd $md; ${tar} -$xf -)'};
        system($cmd) == 0 or die("seutp_testing_maildir: $cmd: $!");
    }
    # 3. (optional) set up .mu under the maildir unless we are told not to
    unless ($ENV{'MUP_MU_HOME'}) {
        my $muhome = "$md/.mu";
        make_path($muhome) or die("setup_testing_maildir: $md/.mu: $!");
        $ENV{'MUP_MU_HOME'} = $muhome;
        warn("seting_testing_maildir: $muhome\n") if $verbose;
        my $mu_opts = $verbose ? '-d --log-stderr ' : '';
        my $bin = $ENV{'MUP_MU_BIN'} || 'mu';
        my $cmd = qq{$bin index ${mu_opts}--muhome=${muhome}};
        warn("indexing: $cmd\n") if $verbose;
        system($cmd) == 0 or die("setup_testing_maildir: $cmd: $!");
    }
}

sub setup_testing_env {
    setup_testing_envars;
    setup_testing_maildir;
}

END {
    remove_tree($ENV{'MAILDIR'},{ verbose => $ENV{'TEST_VERBOSE'} })
        if ($ENV{'MAILDIR'} && $ENV{'MUP_TEST_MAILDIR_SET'} &&
            !$ENV{'MUP_TEST_KEEP_MAILDIR'});
}

setup_testing_env unless $ENV{'MUP_TEST_NO_SETUP'};

1;

##
# Local variables:
# mode: perl
# tab-width: 4
# perl-indent-level: 4
# cperl-indent-level: 4
# cperl-continued-statement-offset: 4
# indent-tabs-mode: nil
# comment-column: 40
# End:
##
