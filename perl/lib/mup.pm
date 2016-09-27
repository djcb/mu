#! perl

=pod

=head1 NAME

mup - perl interface to mu

=head1 SYNOPSIS

  use mup;

  my $mu = mup->new();

  my $results = $mu->find({ subject => 'something'});
  print "$results->{found} results for subject:something\n";

=head1 DESCRIPTION

This is a perl interface to mu, the Maildir search-and-destroy system.
It presents the same API as described in the L<mu-server(1)> man page.
In fact it works by communicating with a C<mu server> process, just
like the C<mu4e> emacs interface to mu does.

=head1 METHODS

All of the following methods take arguments named as described in the
L<mu-server(1)> man page per each command, again either as a single
hashref argument or as a hash of pairs in-line.  If there are any
doubts, make sure to read the L<mu-server(1)> man page.  Where
relevant any C<maildir> argument defaults to C<~/Maildir> (not our
doing, that's just how C<mu> rolls).

In order to stay agnostic with respect to the use our clients put us
to, all exported methods return plain, unblessed hashrefs as their
result.  The shape of this hashref corresponds to the S-Expression
described in the L<mu-server(1)> man page for each command.  Since
everything that comes back from the server is represented as a list
(to us: array), we use a simple heuristic to determine if a an array
in the output is hashrefian or not: if it is of non-zero, even length
and if every even-numbered key is a symbol that starts with a colon we
consider the array hashrefian.  In these cases we perform the obvious
transformation of stripping the leading colon and turning the thing
into a hashref.

=cut

package mup;
use strict;
use warnings;
use vars qw($VERSION);
use Data::SExpression;
use IO::Select;
use IPC::Open2;
use Moose;
use Time::HiRes;
use Data::Dumper;

$VERSION = '0.1.0';

has 'dying' => (
    is => 'rw',
    isa => 'Bool',
    required => 1,
    default => 0
);
has 'dead' => (
    is => 'rw',
    isa => 'Bool',
    required => 1,
    default => 0
);
has 'pid' => (
    is => 'rw',
    isa => 'Int',
    required => 1,
    default => 0
);
has 'in' => (
    is => 'rw'
);
has 'out' => (
    is => 'rw'
);
has 'tout' => (
    is => 'rw',
    isa => 'Num',
    default => 0.5,
    required => 1,
);
has 'orig_tout' => (
    is => 'rw',
    isa => 'Num',
    default => 0.5,
    required => 1,
);
has 'select' => (
    is => 'ro',
    isa => 'Object',
    default => sub { IO::Select->new() },
    required => 1,
);
has 'inbuf' => (
    is => 'rw',
    isa => 'Str',
    default => '',
    required => 1,
);
has 'ds' => (
    is => 'ro',
    isa => 'Object',
    default => sub {
        Data::SExpression->new({ fold_alists => 1, use_symbol_class => 1})
    },
    required => 1,
);
has 'max_tries' => (
    is => 'rw',
    isa => 'Int',
    default => 0,
    required => 1,
);
has 'mu_bin' => (
    is => 'rw',
    isa => 'Str',
    default => 'mu',
    required => 1,
);
has 'mu_server_cmd' => (
    is => 'rw',
    isa => 'Str',
    default => 'server',
    required => 1,
);
has 'verbose' => (
    is => 'rw',
    isa => 'Bool',
    default => 0,
    required => 1,
);
has 'bufsiz' => (
    is => 'rw',
    isa => 'Int',
    default => 2048,
    required => 1,
);
has 'cur_cmd' => (
    is => 'rw',
    isa => 'Str',
    default => '',
    required => 1,
);
has 'maildir' => (
    is => 'rw',
    isa => 'Str',
    default => $ENV{'MAILDIR'} || '',
    required => 1,
);
has 'mu_home' => (
    is => 'rw',
    isa => 'Str',
    default => '',
    required => 1,
);
has 'debug' => (
    is => 'rw',
    isa => 'Bool',
    default => 0,
    required => 1,
);
has 'update_callback' => (
    is => 'rw',
    default => undef,
    required => 0,
);
has 'no_updates' => (
    is => 'rw',
    isa => 'Bool',
    default => 0,
    required => 1,
);

sub _init {
    my $self = shift(@_);
    my($in,$out);
    # The only way I know of to tell mu server what Maildir to use is
    # the MAILDIR environment variable.  If our caller specifies a
    # maildir, set the envvar before we fork the server process.
    if ($self->maildir) {
        $ENV{'MAILDIR'} = $self->maildir;
        warn("mup: setting MAILDIR=".$self->maildir."\n") if $self->verbose;
    }
    # Opposite logic here... a bit confusing: The testing code
    # (c.f. t/lib.pm) wants to point us at a different .mu directory
    # than the default (~/.mu); normally you don't want to do this
    # but if we see a special envar ($MUP_MU_HOME) then set mu_home
    # to this value - mu_home defaults to ''.  In any event, if
    # mu_home is set somehow, obey it, otherwise let mu use its
    # default.
    if ($ENV{'MUP_MU_HOME'}) {
        $self->mu_home($ENV{'MUP_MU_HOME'});
        warn("mup: set --muhome ".$self->mu_home."\n") if $self->verbose;
    }
    # Same for MUP_MU_BIN
    if ($ENV{'MUP_MU_BIN'}) {
        $self->mu_bin($ENV{'MUP_MU_BIN'});
        warn("mup: set mu_bin to ".$self->mu_bin."\n") if $self->verbose;
    }
    my @cmdargs = ($self->mu_bin,$self->mu_server_cmd);
    push(@cmdargs, "--muhome=".$self->mu_home) if $self->mu_home;
    warn("mup: mu server cmd: @cmdargs\n") if $self->verbose;
    my $pid = open2($out,$in,@cmdargs);
    $self->orig_tout($self->tout);
    $self->pid($pid);
    $self->out($out);
    $self->in($in);
    $self->select->add($out);
    my $junk = $self->_read();
    warn("mup: _init junk: $junk\n") if $self->verbose;
    return $self;
}

sub BUILD { shift->_init(); }

sub _cleanup {
    my($self) = @_;
    if ($self->pid) {
        warn("mup: reaping mu server pid ".$self->pid."\n") if $self->verbose;
        waitpid($self->pid,0);
        $self->pid(0);
    }
    if ($self->inbuf) {
        warn("mup: restart pitching inbuf: |".$self->inbuf."|\n")
            if $self->verbose;
        $self->inbuf('');
    }
}

sub restart {
    my($self) = @_;
    $self->_cleanup();
}

sub reset {
    my($self) = @_;
    $self->_reset_parser();
    return $self;
}

sub _read {
    my($self) = @_;
    my $restart_needed = 0;
    my @ready = $self->select->can_read($self->tout);
    while (@ready && !$restart_needed) {
        foreach my $handle (@ready) {
            my $buf = '';
            my $nread = $handle->sysread($buf,$self->bufsiz);
            if (!$nread) {
                warn("mup: mu server died - restarting") if $self->verbose();
                $restart_needed = 1;
            } else {
                $self->inbuf($self->inbuf . $buf);
                warn("mup: <<< |$buf|\n") if $self->verbose;
            }
        }
        @ready = $self->select->can_read($self->tout)
            unless $restart_needed;
    }
    my $result = $self->inbuf;
    $self->_cleanup() if ($self->dying || $restart_needed);
    $self->_init() if $restart_needed && !$self->dying;
    return $result;
}

sub _reset_parser {
}

sub _parse {
    my($self,$in_update) = @_;
    $in_update ||= $self->no_updates;
    my($tries,$max_tries) = (0,$self->max_tries);
  INCOMPLETE:
    my $raw = $self->inbuf;
    return undef unless $raw;
    my($xcount,$left) = ($1,$2) if $raw =~ /^\376([\da-f]+)\377(.*)$/s;
    my $count = hex($xcount);
    my $nleft = length($left);
    warn("mup: count=$count length=$nleft: |$left|\n")
        if $self->verbose;
    if ($count > $nleft) {
        ++$tries;
        die("mup: FATAL: waiting for $count, tried $tries, only got $nleft")
            if ($max_tries && $tries >= $max_tries);
        warn("mup: short buffer, reading more ($tries)...\n")
            if $self->verbose;
        $self->_read();
        goto INCOMPLETE;
    }
    chomp(my $sexp = substr($left,0,$count));
    $self->inbuf(substr($left,$count));
    my $data = $self->ds->read($sexp);
    return undef unless defined($data);
    warn("mup: parsed sexp: $data\n") if $self->verbose;
    my $href = $self->_hashify($data);
    if (!$in_update && $self->update_callback && $self->inbuf =~ /:update/) {
        # We have an update callback.  We have an update.
        # Peanut butter, meet chocolate.
        local $Data::Dumper::Terse = 1;
        local $Data::Dumper::Indent = 0;
        my $upd = $self->_parse(1);
        warn("mup: update: ".Dumper($upd)."\n") if $self->verbose;
        unless ($upd && exists($upd->{'update'})) {
            die("mup: next msg was not pending update !? ".Dumper($upd)."\n");
        }
        &{$self->update_callback}($upd);
    }
    return $href;
    
}

# turn a LISPy keyword into a perlier hashref key

sub _delispify {
    my $key = shift(@_);
    $key = "$1" if "$key" =~ /^:(.*)$/;
    $key =~ s/-/_/g;
    return $key;
}

# turn a perly argument name into a lispy one

sub _lispify {
    my $key = shift(@_);
    $key =~ s/_/-/g;
    return $key;
}

# _hashify - turn raw Data::SExpression result into canonical hashref

sub _hashify {
    my($self,$thing) = @_;
    my $rthing = ref($thing);
    my $result = $thing;
    warn("mup: rthing=$rthing: $thing\n") if $self->debug;
    return $result unless $rthing;
    if ($rthing eq 'Data::SExpression::Symbol') {
        # nil is undef, t is 1, everything else becomes a string
        if ($thing eq 'nil') {
            $result = undef;
        } elsif ($thing eq 't') {
            $result = 1;
        } else {
            $result = "$thing";
        }
    } elsif ($rthing eq 'ARRAY') {
        my $count = scalar(@$thing);
        my $looks_hashrefian = $count && !($count & 1);
        if ($looks_hashrefian) {
            # Non-null array with even cardinality: check for hashrefian keys
            my $i;
            for ($i = 0; $i < $count; $i += 2) {
                my $elt = $thing->[$i];
                my $relt = ref($elt);
                last if (($relt && $relt eq 'Data::SExpression::Symbol') &&
                         "$elt" !~ /^:/);
            }
            $looks_hashrefian = ($i < $count) ? 0 : 1;
            if ($self->debug) {
                local $Data::Dumper::Terse = 1;
                local $Data::Dumper::Indent = 0;
                warn("mup: looks_hashrefian=$looks_hashrefian: ".Dumper($thing)."\n");
            }
        }
        if (!$looks_hashrefian) {
            # just a plain old array
            $result = [ map { $self->_hashify($_) } @$thing ];
        } else {
            # hashref in array's clothing
            $result = {};
            while (scalar(@$thing)) {
                my($key,$val) = splice(@$thing,0,2);
                $key = _delispify($key);
                { no strict 'vars';
                  warn("mup: ARRAY key=$key val=(".ref($val).") |$val|\n")
                      if $self->debug;
                }
                $result->{$key} = $self->_hashify($val);
            }
        }
    } elsif ($rthing eq 'HASH') {
        # xxx can this happen?  Data::SExpression says so but mu will
        # probably never send us one...
        $result = {};
        foreach my $key (keys(%$thing)) {
            my $val = $thing->{$key};
            $key = _delispify($key);
            { no strict 'vars';
            warn("mup: HASH key=$key val=(".ref($val).") |$val|\n")
                if $self->debug;
            }
            $result->{$key} = $self->_hashify($val);
        }
    }
    return $result;
}

=pod

=over 4

=item * new (verbose => 1|0, ... other options... )

Construct a new interface object; this will cause a C<mu server>
process to be started.

Options can be specified Moose-style, either as a hashref
or as a hash of pairs:

=over 4

=item * verbose

If non-zero we spew debug output to C<stderr> via L<warn>.

=item * tout

Timeout in seconds for responses from L<mu-server(1)>.  The
Can be fractional.  The default is C<0.5> (500 msec).

=item * bufsiz

Buffer size for reads from the server.  Default is 2048.

=item * max_tries

Max number of times we will try to read from the server to complete
a single transaction.  By default this is zero, which means no limit.

=item * mu_bin

Name of the C<mu> binary to use to start the server.

=item * mu_server_cmd

C<Mu> subcommand used to start the server.

=item * mu_home

Directory for Mu to use to store the Xapian database and other
Mu-specific files.  Defaults to C<~/.mu>.

=item * maildir

Root of the C<Maildir> tree that L<mu(1)> should operate on.  Defaults
to whatever the C<$MAILDIR> environment variable is set to or
C<~/Maildir> if it is not set.

=item * debug

If non-zero additional debug output will be spewed via L<warn>, mainly
related to the transformation of L<Data::SExpression> objects into
hashrefs.  This output is spewed independently of the value of C<verbose>.

=back

=back

=cut


=pod

=over 4

=item * finish

Shut down the mu server and clean up.

=back

=cut

sub finish {
    my($self) = @_;
    if ($self->pid) {
        $self->dying(1);
        $self->_send("cmd:quit");
        my $junk = $self->_read();
        warn("mup: trailing garbage in finish: |$junk|\n") if $self->verbose;
    }
    return 1;
}

sub DEMOLISH { shift->finish(); }

sub _refify {
    my $href = ((@_ == 1) && (ref($_[0]) eq 'HASH')) ? $_[0] : { @_ };
    return { map { _lispify($_) => $href->{$_} } keys(%$href) };
}

sub _quote {
    my($val) = @_;
    $val = qq|"$val"| if (!ref($val) && $val =~ /\s/);
    $val;
}

sub _argify {
    my $self = shift(@_);
    my $href = _refify(@_);
    if (exists($href->{'timeout'})) {
        $self->tout($href->{'timeout'});
        warn("mup: tout ".$self->orig_tout." => ".$self->tout."\n")
            if $self->verbose;
        delete($href->{'timeout'});
    }
    return join(' ', map { "$_:"._quote($href->{$_}) } keys(%$href));
}

sub _send {
    my($self,$str) = @_;
    $self->in->write("$str\n");
    $self->in->flush();
    return $self;
}

sub _execute {
    my($self,$cmd,@args) = @_;
    my $args = $self->_argify(@args);
    my $cmdstr = "cmd:$cmd $args";
    warn("mup: >>> $cmdstr\n") if $self->verbose;
    if ($self->inbuf) {
        my $junk = $self->inbuf;
        warn("mup: pitching |$junk|\n") if $self->verbose;
    }
    $self->inbuf('');
    $self->cur_cmd($cmd);
    $self->_send($cmdstr);
    $self->_read();
    $self->tout($self->orig_tout);
    return $self->_parse();
}

=pod

=over 4

=item * add (path => "/path/to/file", maildir => "/my/Maildir")

Add a message (document) to the database.  If C<maildir> is not
specified the right thing is filled in.

=back

=cut

sub add {
    my $self = shift(@_);
    my $argref = _refify(@_);
    $argref->{'maildir'} = $self->_our_maildir() unless $argref->{'maildir'};
    $self->_execute('add',$argref);
}



=pod

=over 4

=item * compose (type => 'reply|forward|edit|new', docid => $docid)

Compose a message, either in regard to an existing one (in which case
you must specify C<docid>) or as a new message.

=back

=cut

sub compose { shift->_execute('compose',@_); }



=pod

=over 4

=item * contacts (personal => 1|0, after => $epoch_time)

Search contacts.

=back

=cut

sub contacts { shift->_execute('contacts',@_); }



=pod

=over 4

=item * extract (action => 'save|open|temp', index => $index, path => $path, what => $what, param => $param)

Save a message into a file.

=back

=cut

sub extract { shift->_execute('extract',@_); }



=pod

=over 4

=item * find (query => $mu_query, threads => 1|0, sortfield => $field, reverse => 1|0, maxnum => $max_results)

Search the message Xapian database.

=back

=cut

sub _next {
    my($self) = @_;
    my $href = $self->_parse();
    unless ($href) {
        $self->_read();
        $href = $self->_parse();
    }
    return $href;
}

sub find {
    my($self,@args) = @_;
    my $results = { 'found' => 0, 'results' => [] };
    my $resp = $self->_execute('find',@args);
    return undef unless $resp;
    die("mup: protocol broken!? missing (:erase t)") unless $resp->{'erase'};
    my $result = $self->_next();
    while ($result) {
        if (exists($result->{'found'})) {
            $results->{'found'} = int($result->{'found'});
            $result = undef;
        } elsif (!exists($result->{'docid'})) {
            local $Data::Dumper::Terse = 1;
            local $Data::Dumper::Indent = 0;
            warn("mup: unexpected find result w/no docid: ".Dumper($result));
            $result = undef;
        } else {
            push(@{$results->{'results'}}, $result);
            $result = $self->_next();
        }
    }
    return $results;
}



=pod

=over 4

=item * index (path => $path, my-addresses: 'me,and,mine', callback => $sub)

(Re)index the messagebase.  The mu server updates us with progress
every 500 messages.  By default we only return the final result
after all indexing has occurred but if the caller wants to e.g. update
a progress meter or something it can pass us a special C<callback>
argument that is invoked with every progress report given to us by
the server.

=back

=cut

sub default_maildir { $ENV{'HOME'} . '/Maildir' };

sub _our_maildir {
    return shift->maildir || $ENV{'MAILDIR'} || default_maildir();
}

sub index {
    my $self = shift(@_);
    my $argref = _refify(@_);
    $argref->{'path'} ||= $self->_our_maildir();
    my $cb = $argref->{'callback'};
    delete($argref->{'callback'}) if $cb;
    # The index command is special.  Unlike the others, we don't
    # necessarily send a command and get back a single response.
    # Instead we may get back a series of responses, one for each
    # 500 messages indexed.  Only the last one will be marked with
    # 'status' => 'complete', so wait for that and swallow the rest.
    my $href = $self->_execute('index',$argref);
    return undef unless $href;
    while ($href && $href->{'status'} ne 'complete') {
        my($status,$pr,$up,$cl) =
            map { $href->{$_} } qw(status processed updated cleaned_up);
        warn("mup: index $status: $pr processed, $up updated, $cl cleaned\n")
            if $self->verbose;
        &$cb($href) if $cb;
        $self->_read();
        my $tmp = $href;
        do {
            $tmp = $self->_parse(); # can call _read()
            &$cb($tmp) if $cb;
            $href = $tmp if $tmp;
        } while ($tmp && $tmp->{'status'} ne 'complete');
    }
    return $href;
}



=pod

=over 4

=item * mkdir (path => $path)

Make a new maildir under your Maildir basedir.

=back

=cut

sub mkdir {
    my $self = shift(@_);
    my $argref = _refify(@_);
    # Make path relative to our Maildir unless it is absolute
    my $path = $argref->{'path'};
    die("mup: path is required") unless $path;
    $path = join("/",$self->_our_maildir(),$path) unless $path =~ /^\//;
    warn("mup: mkdir ".$argref->{'path'}." => mkdir $path\n")
        if $self->verbose;
    $argref->{'path'} = $path;
    $self->_execute('mkdir',$argref);
}


=pod

=over 4

=item * move ( docid => $docid | msgid => $msgid, maildir => $path, flags => $flags)

Move a message from one maildir folder to another.

=back

=cut

sub move {
    my $self = shift(@_);
    my $argref = _refify(@_);
    # Unlike mkdir's path arg, our maildir arg must start with a slash
    my $maildir = $argref->{'maildir'};
    $argref->{'maildir'} = "/$maildir" unless $maildir =~ /^\//;
    $self->no_updates(1);
    my $result = $self->_execute('move',$argref);
    $self->no_updates(0);
    return $result;
}

=pod

=over 4

=item * ping ()

Ping the server to make sure it is alive.

=back

=cut

sub ping { shift->_execute('ping',@_); }



=pod

=over 4

=item * remove (docid => $docid)

Remove a message by document ID.

=back

=cut

sub remove { shift->_execute('remove',@_); }



=pod

=over 4

=item * view ( docid => $docid | msgid => $msgid | path => $path, extract_images => 1|0, use_agent => 1|0, auto_retrieve_key => 1|0)

Return a canonicalized view of a message, optionally with images
and/or cryptography (PGP) dealt with.  The message can be specified by
C<docid>, C<msgid> or as a path to a file containing the message.

=back

=cut

sub view { shift->_execute('view',@_); }

########################################################################

no Moose;
__PACKAGE__->meta->make_immutable;

1;

__END__

=pod

=head1 SEE ALSO

L<mu(1)>, L<mu-server(1)>

=head1 AUTHOR

attila <attila@stalphonsos.com>

=head1 LICENSE

Copyright (C) 2015 by attila <attila@stalphonsos.com>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

=cut

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
